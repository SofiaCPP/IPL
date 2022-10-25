EXAMPLE=$1

if [ -z "$EXAMPLE" ]
then
  echo "[Build] Compiling All Examples"
  for EXAMPLE in */
  do
    g++ ./$EXAMPLE/main.cpp ../src/*.cpp -o ./$EXAMPLE/rb -Wall -Werror
  done
  echo "[Build] Finished"
else
  echo "[Build] Compiling Example: $EXAMPLE"
  g++ ./$EXAMPLE/main.cpp ../src/*.cpp -o ./$EXAMPLE/rb -Wall -Werror
  echo "[Build] Finished"
fi
