ls run-lisp
echo "Creating symlink for run-lisp"
if [ $? -eq 0 ]; then
    exit 0
else
    ln -s .stack-work/dist/x86_64-linux/ghc-9.6.6/build/lisp-exe/lisp-exe run-lisp
fi