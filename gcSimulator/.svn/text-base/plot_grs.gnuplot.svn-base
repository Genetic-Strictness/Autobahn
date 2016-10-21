binwidth=1
bin(x,width)=width*floor(x/width) + binwidth/2.0
set boxwidth binwidth

plot  'test.dat' using (bin($1,binwidth)):1.0 smooth freq with boxes
