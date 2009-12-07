int main() {
	int i;
	i=0;
	while (i < 10) {
	  for(int j = i; j < 10; j++)
	  {
		for(int k = j; k < 10; k++)
	  	{
			printf("AA");
	  	}
		printf("AA");
	  }
	  for(int m = 0; m < 10; m++)
	  {
		printf("AA");
	  }
	  i++;
	}
	return 0;
}

