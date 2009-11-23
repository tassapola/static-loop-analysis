int main() {
  int i,j,k,a,b;
	i=0;
	a=1;
	b=7;
	j=a+b;
	k=10;

	while (j < k *2 ) {
          j = j + 5;
        }

	while (i < 10) {
	  printf("%d\n",i);
	  i = i + a;
	}

	k = i;
	return 0;
}

