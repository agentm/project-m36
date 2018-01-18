# Run tests
if [[ "$(uname)" = "Darwin" ]]; then
    timeout_cmd="gtimeout"
else
    timeout_cmd="timeout"
fi
$timeout_cmd 30m stack --no-terminal --jobs=2 --install-ghc --system-ghc build --only-dependencies --fast
ret=$?
case "$ret" in
    0)
    # continue
    ;;
    124)
        echo "Timed out while installing dependencies."
        echo "Try building again by pushing a new commit."
        exit 0
        ;;
    *)
        echo "Failed to install dependencies; stack exited with $ret"
        exit "$ret"
        ;;
esac
