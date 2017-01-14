//Compiler implementation (move command)
//Refer to the contents of the data sheet and place it in the re and i tables.

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#include <stdlib.h>
struct reg
{
	char reg_name[3];
	char reg_num[4];
} Reg[20];     
// Structure that stores names and numbers for registers
struct ins
{
	char instruct[6];
	char dest[2];
	char sour[2];
	char word_type[2];
	char ins_code[3];
	char ins_len[2];
	char mod_reg[50];
} Instr[100],Instr_tmp;
// A structure that holds information about each instruction

int MaxI;

struct symbol_tbl
{
	char symbol[10];
	char word_type[2];
	int lc;
	char data[10];
} Symbol[20];     // Symbol table

int MaxS;

struct sentence
{
	char label[10];
	char _operator[10];
	char operand[3][10];
} Sen;

int LC;

void Initialize()
{
	int i=0, j=1;
	FILE *regi, *inst;
	regi=fopen("reg_tbl.txt","r");
	inst=fopen("inst_tbl.txt","r");
	// Read information of registers and instruction
	
	while(!feof(regi))
	{
		fscanf(regi,"%s%s\n", Reg[i].reg_name, Reg[i].reg_num);
		i++;
	}     // write registers table
	while(!feof(inst))
	{
		fscanf(inst, "%6s%2s%2s%4s%3s%2s%9s\n", Instr[j].instruct, 
			Instr[j].dest, Instr[j].sour, 
			Instr[j].word_type, Instr[j].ins_code, 
			Instr[j].ins_len, Instr[j].mod_reg);
		j++;
	}     // write command table
	MaxI=j-1;
	fclose(regi);
	fclose(inst);
}
int Analyze(char *operand)
{
	int i=0;
	char *regist[]={"AX","BX","CX","DX","SP","BP","SI","DI"
		,"ES","CS","SS","DS","AL","BL","CL","DL","AH","BH","CH","DH",0x00}; //registers

	if(isdigit(operand[0])) return 0;      // set the address mode
	else
		while(regist[i]!=0x00)
			if(!strcmp(operand,regist[i]))
			{

				if(i>0&&i<4)       return 1; //BX...
				else if(11<i)      return 2; //AL,BL...
				else if(7<i&&i<12) return 4; //segment registers
				else if(i==0)      return 5; //Accumulate  
			}
			else i++;
			return 3; //if memory
}
//Get the number of bytes in an operation. 
//16high=return3 16=return 1 8=return2 immediate=return 0 

#define MAX_INS 1     // set the initial number of command
void Check(struct ins instr,int i)
{
	if(i==0){
		if(instr.dest=="r"&&instr.word_type=="w") exit(1);
	}
}
int Add_Chk(char *sen)  
{
	register int k=MaxI;
	int i=0,j=0,l=0,wp=0;
	char op[5][10],*opcode[]={"mov",""};
	while(sen[wp]!='\n')
	{
		while(sen[wp]==' '||sen[wp]=='\t'||sen[wp]==',') wp++; // pass the Spaces, tabs, and comma

		while(sen[wp]!=' '&&sen[wp]!='\t' &&sen[wp]!='\n' &&sen[wp]!=',')
		{
			*(op[j]+i)=sen[wp];
			i++;
			wp++;
		}
		*(op[j]+i)='\0';
		i=0;
		j++;
	}
	i=0;
	while(strcmp(opcode[i],""))
		if(stricmp(opcode[i],op[0]))i++;
		else
		{
			strcpy(Sen._operator,op[0]);
			for(l=1; l<j; l++) strcpy(Sen.operand[l-1],op[l]);
			break;
		}
		if(i==MAX_INS)
		{
			strcpy(Sen.label,op[0]);
			strcpy(Sen._operator,op[1]);

			for(l=2;l<j;l++) strcpy(Sen.operand[l-2],op[1]);
		}     // Analyze one sentence and classify it into label, operator and operand
		strcpy(Instr[0].instruct,op[0]);     // store OPcode
		switch(Analyze(op[1]))
		{
		case 0: strcpy(Instr[0].dest,"i");
				break;
		
		case 1: strcpy(Instr[0].dest,"r");
				strcpy(Instr[0].word_type,"w");
				break;
		
		case 2: strcpy(Instr[0].dest,"r");
				strcpy(Instr[0].word_type,"b");
				break;
	
		case 3: strcpy(Instr[0].dest,"m");
				strcpy(Instr[0].word_type,"w");break;
				
		case 4: strcpy(Instr[0].dest,"s"); 
			    strcpy(Instr[0].word_type,"0");break;

		case 5: strcpy(Instr[0].dest,"r");
				strcpy(Instr[0].word_type,"w");
				break;
		
		}
		switch(Analyze(op[2]))
		{
		case 0: strcpy(Instr[0].sour,"i");
				break;
		
		case 1:	strcpy(Instr[0].sour,"r");         	
				if(Analyze(op[1])==2) exit(1);     // AL BX
				break;//Instr[0].wordtype=w 
		
		case 2: strcpy(Instr[0].sour,"r");
				if(Analyze(op[1])==1) exit(1);     //AX BL
				break;//Instr[0].wordtype=b
		
		case 3: strcpy(Instr[0].sour,"m");break;
		
		case 4: strcpy(Instr[0].sour,"s");
			    strcpy(Instr[0].word_type,"0");
				if(Analyze(op[1])==4) exit(1);    //SS DS
				break;

		case 5: strcpy(Instr[0].sour,"r");         	
				if(Analyze(op[1])==2) exit(1);     // AL BX
				break;//Instr[0].wordtype=w 
		}


		while(stricmp(Instr[k].instruct,Instr[0].instruct)
			||strcmp(Instr[k].dest,Instr[0].dest)
			||strcmp(Instr[k].sour,Instr[0].sour)
			||strcmp(Instr[k].word_type,Instr[0].word_type))
			k--;
		return k;
}
void PassI(char *buf)
{
	int i;
	static int j=0;
	i=Add_Chk(buf);

	if(i)
	{
		printf("%04X: %s",LC,buf);
		if(!strcmp(Instr[i].dest,"m") || !strcmp(Instr[i].sour,"m"))
			//If the operation has memory, the instruction length + memory size
		{
			if(!strcmp(Instr[i].word_type,"w"))
				LC+=atoi(Instr[i].ins_len)+2; //if memory dw
			else 
				LC+=atoi(Instr[i].ins_len)+1;//if memory db
		}
		else
		LC+=atoi(Instr[i].ins_len);
	}
	else
	{
		if(!stricmp(Sen._operator,"dw"))//Symbol processing 
			strcpy(Symbol[j].word_type,"w");
		else if(!stricmp(Sen._operator,"db"))
			strcpy(Symbol[j].word_type,"b");
		strcpy(Symbol[j].symbol,Sen.label);
		strcpy(Symbol[j].data,Sen.operand[0]);
		Symbol[j].lc=LC;
		printf("%04X: %s",LC,buf);
		printf("%c ",*Symbol[j].word_type);
		if(*Symbol[j].word_type =='w')   LC+=2;
		else if(*Symbol[j].word_type == 'b') LC+=1;
		j++;
	}
}
int btoi(char *dig)
{
	register int i=0, ret=0;
	while(*(dig+i)!='\0')
	{
		if(*(dig+i)=='1') ret+=pow((double)2,(double)strlen(dig+i)-1);
		i++;
	}
	return ret;
}
void PassII(char *buf)
{
	int i=0, j=0, k=0;

	i=Add_Chk(buf);
	Instr_tmp=Instr[i]; 
	
	if(i)
	{
		printf("%04x: %3s ", LC, Instr[i].ins_code); //8A
		if(!strcmp(Instr[i].dest,"r"))//If it is a register, compare it in the register table.[R ?]
		{
			if(!strcmp(Instr[i].sour,"r"))//[R R]
			{
			while(stricmp(Reg[j].reg_name,Sen.operand[0]))//Find the register number ex) ax 000
				j++;
			strncpy(strchr(Instr[i].mod_reg,'?'),Reg[j].reg_num,3);
			j=0;
		
			while(stricmp(Reg[j].reg_name,Sen.operand[1]))
				j++;
			strncpy(strchr(Instr[i].mod_reg,'?'),Reg[j].reg_num,3);
			printf("%02X     %s",btoi(Instr[i].mod_reg),buf);
			}//MOV AX, BX
		
			if(!strcmp(Instr[i].sour,"s"))//[R S]
			{
				while(stricmp(Reg[j].reg_name,Sen.operand[1]))
				j++;
				strncpy(strchr(Instr[i].mod_reg,'?'),Reg[j].reg_num,2);
				j=0;
				while(stricmp(Reg[j].reg_name,Sen.operand[0]))
				j++;
				strncpy(strchr(Instr[i].mod_reg,'?'),Reg[j].reg_num,3);
				printf("%02X     %s",btoi(Instr[i].mod_reg),buf);
			}
			
			if(!strcmp(Instr[i].sour,"m"))//[R M]
			{
				//while(strcmp(Symbol[k].symbol,Sen.operand[1]))k++; //mov r m
			//while(stricmp(Reg[j].reg_name,Sen.operand[0]))
				//j++;
				while(stricmp(Reg[j].reg_name,Sen.operand[0]))j++;

				strncpy(strchr(Instr[i].mod_reg,'?'),Reg[j].reg_num,3);
				strncpy(strchr(Instr[i].mod_reg,'?'),"110",3);
		        j=0;
				printf("%02X %04X %s", btoi(Instr[i].mod_reg),Symbol[k].lc,buf);//output ???
			}	
		}

		if(!strcmp(Instr[i].dest,"s"))//[S ?]
		{
			if(!strcmp(Instr[i].sour,"r"))//[S R]
			{
				while(stricmp(Reg[j].reg_name,Sen.operand[0]))
				j++;
				strncpy(strchr(Instr[i].mod_reg,'?'),Reg[j].reg_num,2);
		        j=0;
				while(stricmp(Reg[j].reg_name,Sen.operand[1]))
				j++;
				strncpy(strchr(Instr[i].mod_reg,'?'),Reg[j].reg_num,3);
		        j=0;
				printf("%02X     %s",btoi(Instr[i].mod_reg),buf);
			}


		}
		/*if(strcmp(Instr[i].dest,"m")&&strcmp(Instr[i].sour,"m")){
			printf("%02X %s",btoi(Instr[i].mod_reg),buf);
			Instr[i]=Instr_tmp;
		}//pass2 output*/
		if(!strcmp(Instr[i].dest,"m"))
		{
				//while(strcmp(Symbol[k].symbol,Sen.operand[0]))
					//k++;//if correct, out. mov m r
			while(stricmp(Reg[j].reg_name,Sen.operand[0]))j++;
			strncpy(strchr(Instr[i].mod_reg,'?'),Reg[j].reg_num,3);
			strncpy(strchr(Instr[i].mod_reg,'?'),"110",3);
			//while(strcmp(Symbol[k].symbol,Sen.operand[1]))k++; //mov r m
			//while(stricmp(Reg[j].reg_name,Sen.operand[0]))
				//j++;
			
			printf(" %02X %04X %s", btoi(Instr[i].mod_reg),Symbol[k].lc,buf);//output ???
		}
		
		if(i) //output
		{
			if(!strcmp(Instr[i].dest,"m") || !strcmp(Instr[i].sour,"m"))
			{
				if(!strcmp(Instr[i].word_type,"w"))
					LC+=atoi(Instr[i].ins_len)+2; //if memory dw
				else 
					LC+=atoi(Instr[i].ins_len)+1;//if memory db
			}
			else
			LC+=atoi(Instr[i].ins_len);
		}
	}

	else
	{
		k=0;
		while(strcmp(Symbol[k].symbol,Sen.label))k++;
		if(!strcmp(Symbol[k].word_type,"w"))
			printf("%04X:%04X %20s", LC, atoi(Symbol[k].data),buf);
		if(!strcmp(Symbol[k].word_type,"b"))
			printf("%04X: %02X %20s",LC,atoi(Symbol[k].data),buf);
		if(*Symbol[k].word_type == 'w')  LC+=2;
		else if(*Symbol[k].word_type == 'b') LC+=1;
	}

}
void main()
{
	char buf[50];
	FILE *in;
	in=fopen("test1.asm", "r");
	Initialize();
	printf("\nPass1:\n");
	while(1)
	{
		fgets(buf,30,in);
		if(feof(in))break;
		PassI(buf);
	}
	rewind(in);
	LC=0;
	printf("\nPass2:\n");
	
	while(1)
	{
		fgets(buf,30,in);
		if(feof(in)) break;
		PassII(buf);
	}
	
	fclose(in);
}