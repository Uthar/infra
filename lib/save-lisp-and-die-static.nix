{ pkgs
, lib
, sbclStatic
, pname ? "app"
, version ? ""
, ... }:

let
  dump-linkage-info =
    "${sbclStatic}/tools-for-build/dump-linkage-info.lisp";
  create-linkage-table-prelink-info-override =
    "${sbclStatic}/tools-for-build/create-linkage-table-prelink-info-override.lisp";
in pkgs.runCommand "${pname}${lib.optionalString (version != "") "-"}${version}" {} ''

 mkdir -pv $out/bin

 ${sbclStatic}/bin/sbcl \
   --non-interactive --no-sysinit --no-userinit \
   --eval '(defun main () (print :hello) (terpri))' \
   --load ${dump-linkage-info} \
   --eval '(sb-dump-linkage-info:dump-to-file "linkage-info.sexp")' \
   --eval '(sb-ext:save-lisp-and-die "core")'

 ${sbclStatic}/bin/sbcl --no-sysinit --no-userinit \
   --script ${create-linkage-table-prelink-info-override} \
   linkage-info.sexp linkage-table-prelink-info-override.c

 ${sbclStatic.stdenv.cc}/bin/gcc -Wno-builtin-declaration-mismatch \
   -o linkage-table-prelink-info-override.o \
   -c linkage-table-prelink-info-override.c

 ${sbclStatic.stdenv.cc}/bin/gcc -no-pie -static \
   -o static-sbcl  \
   ${sbclStatic}/lib/sbcl/sbcl.o  \
   linkage-table-prelink-info-override.o \
   -L${pkgs.zlib.static}/lib/ \
   -ldl -lpthread -lz  -lm

 ./static-sbcl --core core \
             --non-interactive \
             --no-sysinit --no-userinit \
             --eval '(sb-ext:save-lisp-and-die "${pname}" :executable t :toplevel (lambda () (main)) :compression t)'

 cp ${pname} $out/bin

''
