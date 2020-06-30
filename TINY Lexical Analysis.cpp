#include <iostream>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#define sfName "source.txt"
#define tfName "target.txt"
#define MAXTOKENLEN 200
using namespace std;
FILE *source;
FILE *target;
char* Keys[]={"if","then","else","end","repeat","until","read","write","or","and","int","bool","char","while","do"};
int key_num=15;
typedef enum{
	ENDFILE,ERROR,//0-1
	ID,NUM,STR,//2-4
	ASSIGN,EQ,LT,BT,LET,BET,PLUS,MINUS,TIMES,OVER,LPAREN,RPAREN,SEMI,COMMA//SYM
}TokenType;
typedef enum{
	START,INASSIGN,INCOMMENT,INNUM,INID,INSTR,INLT,INBT,DONE
}StateType;
char get_next_char(){
	char ch;
	ch=fgetc(source);
	return ch;
}
void unget_next_char(){
	if(!feof(source)) fseek(source,-1,SEEK_CUR);
}
int getToken(){
	int state=START;
	int currentToken;
	string tokenString;
	int tokenStringIndex=0;
	while(state!=DONE){
		char ch=get_next_char();
		bool save=true;
		switch(state){
			case START:
				if(isdigit(ch)) state=INNUM;
				else if(isalpha(ch)) state=INID;
				else if(ch==':') state=INASSIGN;
				else if(ch=='{'){
					save=false;
					state=INCOMMENT;
				}
				else if(ch=='\''){
					save=false;
					state=INSTR;
				}
				else if(ch==' '||ch=='\t'||ch=='\n') save=false;
				else if(ch=='<') state=INLT;
				else if(ch=='>') state=INBT;
				else{
					state=DONE;
					switch(ch){
						case EOF:save=false;currentToken=ENDFILE;break;
						case '=':currentToken=EQ;break;
						case '+':currentToken=PLUS;break;
						case '-':currentToken=MINUS;break;
						case '*':currentToken=TIMES;break;
						case '/':currentToken=OVER;break;
						case '(':currentToken=LPAREN;break;
						case ')':currentToken=RPAREN;break;
						case ';':currentToken=SEMI;break;
						case ',':currentToken=COMMA;break;
						default:currentToken=ERROR;break;
					}
				}
				break;
			case INCOMMENT:
				save=false;
				if(ch=='}') state=START;
				break;
			case INSTR:
				if(ch=='\''){
					save=false;
					currentToken=STR;
					state=DONE;
				}
				if(ch==EOF){
					save=false;
					currentToken=ERROR;
					state=DONE;
				}
				break;
			case INASSIGN:
				state=DONE;
				if(ch=='=') currentToken=ASSIGN;
				else{
					unget_next_char();
					save=false;
					currentToken=ERROR;
				}
				break;
			case INLT:
				state=DONE;
				if(ch=='=') currentToken=LET;
				else{
					unget_next_char(); 
					save=false;
					currentToken=LT;
				}
				break;
			case INBT:
				state=DONE;
				if(ch=='=') currentToken=BET; 
				else{
					unget_next_char(); 
					save=false;
					currentToken=BT;
				}
				break;
			case INNUM:
				if(!isdigit(ch)){
					unget_next_char();
					save=false;
					state=DONE;
					currentToken=NUM;
				}
				break;
			case INID:
				if(!isalpha(ch)&&!isdigit(ch)){
					unget_next_char();
					save=false;
					state=DONE;
					currentToken=ID;
				}
				break;
			case DONE:
			default:
				cout<<"Bug state"<<state<<endl;
				state=DONE;
				currentToken=ERROR;
				break;
		}
	if(save&&tokenStringIndex<=MAXTOKENLEN) {
		tokenStringIndex++;
		tokenString+=ch;
	}
	}
	string x;
	if(currentToken>4) x="SYM";
	else if(currentToken==3) x="NUM";
	else if(currentToken==4) x="STR";
	else if(currentToken==1) x="ERROR";
	else if(currentToken==0) x="ENDFILE";
	else if(currentToken==2){
		int lab=0;
		x="ID";
		for(int i=0;i<key_num;i++){
			if(Keys[i]==tokenString){
				lab=1;
				break;
			}
		}
		if(lab==1) x="KEY";
	}
	string re=x+" "+tokenString;
	cout<<re<<endl;
	fprintf(target,"%s\n",re.c_str());
	return currentToken;
}

int main(){
	source=fopen(sfName,"r");
	target=fopen(tfName,"w");
	while(getToken()!=ENDFILE);
    fclose(source);
    fclose(target);
    return 0;
}

