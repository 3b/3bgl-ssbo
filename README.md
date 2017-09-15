### 3bgl-ssbo: utilities for writing data into an OpenGL SSBO

Currently assumes [3bgl-shader](https://github.com/3b/3bgl-shader/),
but should work with anything that can produce shader descriptions in
input or output format of [glsl-packing](https://github.com/3b/glsl-packing/).

API isn't finalized yet, so things might be changed and/or renamed.

Currently uses named immutable buffer API, so needs GL 4.5, though
support for older APIs will probably be added (separate bind function
per API, or option to single `bind-ssbo` function to select API?
Possibly wouldn't slow it down to query GL version at buffer
creation or in `make-ssbo`, allowing automatic switching?).

TODO: add UBO support

### Minimal example:

```lisp
;; simple shader

(cl:defpackage 3bgl-ssbo-shader
  (:use :3bgl-glsl/cl))
(cl:in-package 3bgl-ssbo-shader)

(input position :vec4 :location 0)

(interface globals (:buffer t :layout (:binding 0 :std430 t))
  (foo :int)
  (mvp :mat4)
  (vec :vec4))

(defun vertex ()
  (setf gl-position (* mvp position)))

```

```lisp

(defconstant +globals-binding+ 0)
;; `3bgl-ssbo` currently only supports hash tables for input data
;; but will support at least CLOS at some point, possibly `defstruct`.
(let* ((data (make-hash-table))
       (ssbo (3bgl-ssbo:make-ssbo :data data)))

  ;; store some data
  (setf (gethash '3bgl-ssbo-shader::vec data) (sb-cga:vec 1.0 2.0 3.0))
        ;; matrix slots currently must set from sequence with same
        ;; number of elements, but eventually should support filling
        ;; any size matrix from a 4x4 matrix
        (gethash '3bgl-ssbo-shader::mvp data) (sb-cga:identity-matrix)


;;; Get shader metadata while building shader stages:
  (multiple-value-bind (source uniforms attributes ssbos structs)
      (3bgl-shaders::generate-stage :vertex '3bgl-ssbo-shader::vertex
                                    :expand-uniforms t)
    (declare (ignore source attributes uniforms))
    ;; update the ssbo layout whenever the shader is recompiled.
    ;; :INDEX 0 tells it to use whatever buffer is at binding 0. Use
    ;; :NAME instead of :INDEX to specify buffer name directly.

    ;; in real code we would probably combine SSBOS and STRUCTS for
    ;; all stages in a shader program instead of just using one stage
    (3bgl-ssbo:update-ssbo-layout ssbo (3bgl-ssbo:calculate-layout
                                        ssbos structs
                                        :index +globals-binding+)))


  ;; main draw loop
  (loop
    while *looping*
    do
       ;; before using shader bind the ssbo, which also handles
       ;; creating/updating the buffer object in GL API
       (3bgl-ssbo:bind-ssbo ssbo +globals-binding+)

       ;; ... draw stuff

       ;; when updating data, flag ssbo as dirty so it will be
       ;; uploaded at next bind.
       (when (view-changed)
         (setf (gethash '3bgl-ssbo-shader::mvp data) (get-mvp-matrix))
         (setf (3bgl-ssbo:dirty ssbo) t))))

```


SSBO also supports variable-sized buffers, where the last slot of the
buffer is an array with no size specified (`some-type foo[];` in glsl,
`(foo (some-type :*)` in `3bgl-shaders`).

In that case, the corresponding entry in `data` should be a sequence
of hash tables (or objects when supported in general) containing data
for each element of the final array slot. If the object has a slot to
store the length of the final array, pass `:count-slot 'name` to
`make-ssbo`, and slot will be filled automatically with current size.

Storing structs in slots works similarly, with a hash table in the
entry in `data`.

Fixed-size arrays aren't implemented yet, but will eventually work
like variable-sized arrays.