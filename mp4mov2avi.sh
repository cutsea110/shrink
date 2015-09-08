pushd $1;

for f in `ls *.MP4 *.MOV *.mp4 *.mov`; do
  echo ffmpeg -y -i ${f} -vcodec mjpeg -acodec libmp3lame -s hd720 ${f%.*}.avi;
  ffmpeg -y -i ${f} -vcodec mjpeg -acodec libmp3lame -s hd720 ${f%.*}.avi;
done

popd;

