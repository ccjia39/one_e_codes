dir=$(pwd)
direx=$(pwd)'/Example/'
echo $dir
echo $direx

cd $direx
sed "s#dir#$dir#g" getsta_h.tmp > getsta_h.xml
$dir/GetSta getsta_h
sed "s#dir#$dir#g" getsta_nitrogen.tmp > getsta_nitrogen.xml
$dir/GetSta getsta_nitrogen
$dir/Coll collinput
$dir/Sigma
cd ..

