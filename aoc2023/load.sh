for i in {2..25}
do
     day=$(printf "%02d" "$i")
     cat day01.scala | sed "s/day01/day$day/g" > "day$(printf "%02d" "$i").scala"
done
