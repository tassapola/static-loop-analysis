int main() {
  int i,j,k,a,b;
	i=0;
	a=1;
	b=7;
	j=a+b;
	
	while (i < 1000) {
	  for (j=0;j<10000;j++) {
	    printf("test\n");
	    i += 1;
	  }
	  i = i + a;
	}

	k = i;
	return 0;
}

