EXAMPLE=$1
SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

if [ -z "$EXAMPLE" ]
then
  echo "[Build] Compiling All Examples"
  for EXAMPLE in */
  do
    g++ $SCRIPT_DIR/$EXAMPLE/main.cpp $SCRIPT_DIR/../src/**/*.cpp -o $SCRIPT_DIR/$EXAMPLE/rb -Wall -Werror
  done
  echo "[Build] Finished"
else
  echo "[Build] Compiling Example: $EXAMPLE"
  g++ $SCRIPT_DIR/$EXAMPLE/main.cpp $SCRIPT_DIR/../src/**/*.cpp -o $SCRIPT_DIR/$EXAMPLE/rb -Wall -Werror
  echo "[Build] Finished"
fi
