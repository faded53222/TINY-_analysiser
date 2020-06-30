#include <iostream>
#include <sstream>
#include <vector>
#include <stdio.h>
#include <stdlib.h>
#include <string>
#define sfName "source.txt"
#define MAXTOKENLEN 200
using namespace std;
struct node{
	int token;
	string token_string;
	vector<node*>son;
	vector<string> code_keep; 
	node* father;
	int cnt;
	int disabled=0;
	node* addSon(int token,string token_string){
		node* Son=new node();
		Son->father=this;
		Son->token=token; 
		Son->token_string=token_string;
		son.push_back(Son);
		return Son;
	}
} *Root;
node* current_node;
node* root_node=new node();
FILE *source;
typedef enum{
	ENDFILE,ERROR,//0-1
	ID,NUM,STR,//2-4
	ASSIGN,EQ,LT,BT,LET,BET,PLUS,MINUS,TIMES,OVER,LPAREN,RPAREN,SEMI,COMMA,//SYM 5-18
	IF,THEN,ELSE,END,REPEAT,UNTIL,READ,WRITE,OR,AND,INT,BOOL,CHAR,WHILE,DO
}TokenType;
char* Keys[]={"if","then","else","end","repeat","until","read","write","or","and","int","bool","char","while","do"};
int key_num=15;
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
vector<int> tokens;
vector<string> tokens_string;
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
	if(currentToken==ID){
		for(int i=0;i<key_num;i++){
			if(Keys[i]==tokenString){
				currentToken=IF+i;
				break;
			}
		}
	}
	if(currentToken!=ENDFILE){
		tokens.push_back(currentToken);
		tokens_string.push_back(tokenString);
	}
	return currentToken;
}
int errors_num;
vector<int> errors_num_keep;
vector<string> errors;
int sym;
int count=0;
vector<int> count_keep;
int lv=-1;
int count_t=0,count_l=0;

vector<string> s_l;
void get_exp(node* root){
	if(root->token>0 && root->token!=ASSIGN 
	&& root->token!=READ && root->token!=WRITE
	&& root->token!=IF && root->token!=WHILE){
		s_l.push_back(root->token_string);
	}
	for(int i=0;i<root->son.size();i++) 
        if(root->son[i]->disabled==0)
    		get_exp(root->son[i]);	
}
string new_name(){
	count_t+=1;
	stringstream ss;
  	ss<<count_t;
  	return "t"+ss.str();
}
string new_label(){
	count_l+=1;
	stringstream ss;
  	ss<<count_l;
  	return "L"+ss.str();
}
vector<string> result;
vector<string> end_names;
vector<string> start_names;
void deal(node* root){
	lv++;
	string tr;
	s_l.clear();
	if(root->token_string=="assign-stmt"){
		get_exp(root);
		while(s_l.size()!=2){
			string name=new_name();
			int t_k;
			for(int j=0;j<2;j++)
				for(int i=s_l.size()-1;i>=0;i--){
					if(((s_l[i]=="*"||s_l[i]=="/")&&j==0)||((s_l[i]=="+"||s_l[i]=="-")&&j==1)){
						result.push_back(name+":="+s_l[i-1]+s_l[i]+s_l[i+1]);
						s_l[i-1]=name;
						s_l.erase(s_l.begin()+i+1);
						s_l.erase(s_l.begin()+i);
						break;
					}
				}
		}
		result.push_back(s_l[0]+":="+s_l[1]);
	}
	else if(root->token_string=="read-stmt"){
		get_exp(root);
		result.push_back("read "+s_l[0]);
	}
	else if(root->token_string=="write-stmt"){
		get_exp(root);
		result.push_back("write "+s_l[0]);
	}
	else if(root->token_string=="if-stmt"){
		get_exp(root);
		result.push_back("Label "+new_label());
		if(s_l[3]=="and"){
			string name1=new_label(),name2=new_label(),end_name=new_label();
			end_names.push_back(end_name);
			result.push_back("if "+s_l[0]+s_l[1]+s_l[2]+" goto "+name1);
			result.push_back("goto "+end_name);
			result.push_back("Label "+name1);
			result.push_back("if "+s_l[4]+s_l[5]+s_l[6]+" goto "+name2);
			result.push_back("goto "+end_name);
			result.push_back("Label "+name2);
		}
		else if(s_l[3]=="or"){
			string name1=new_label(),end_name=new_label();
			end_names.push_back(end_name);
			result.push_back("if "+s_l[0]+s_l[1]+s_l[2]+" goto "+name1);
			result.push_back("if "+s_l[4]+s_l[5]+s_l[6]+" goto "+name1);
			result.push_back("goto "+end_name);
			result.push_back("Label "+name1);
		}
		else{
			string name1=new_label(),end_name=new_label();
			end_names.push_back(end_name);
			result.push_back("if "+s_l[0]+s_l[1]+s_l[2]+" goto "+name1);
			result.push_back("goto "+end_name);
			result.push_back("Label "+name1);
		}
	}
	else if(root->token_string=="while-stmt"){
		get_exp(root);
		result.push_back("Label "+new_label());
		string start_name=new_label();
		start_names.push_back(start_name);
		result.push_back("Label "+start_name);
		if(s_l[3]=="and"){
			string name1=new_label(),name2=new_label(),end_name=new_label();
			end_names.push_back(end_name);
			result.push_back("if "+s_l[0]+s_l[1]+s_l[2]+" goto "+name1);
			result.push_back("goto "+end_name);
			result.push_back("Label "+name1);
			result.push_back("if "+s_l[4]+s_l[5]+s_l[6]+" goto "+name2);
			result.push_back("goto "+end_name);
			result.push_back("Label "+name2);
		}
		else if(s_l[3]=="or"){
			string name1=new_label(),end_name=new_label();
			end_names.push_back(end_name);
			result.push_back("if "+s_l[0]+s_l[1]+s_l[2]+" goto "+name1);
			result.push_back("if "+s_l[4]+s_l[5]+s_l[6]+" goto "+name1);
			result.push_back("goto "+end_name);
			result.push_back("Label "+name1);
		}
		else{
			string name1=new_label(),end_name=new_label();
			end_names.push_back(end_name);
			result.push_back("if "+s_l[0]+s_l[1]+s_l[2]+" goto "+name1);
			result.push_back("goto "+end_name);
			result.push_back("Label "+name1);
		}
	}
	else if(root->token_string=="end"){
		if(start_names.size()>0){
			result.push_back("goto "+start_names.back());
			start_names.pop_back();
		}
		result.push_back("Label "+end_names.back());
		end_names.pop_back();
	}
	for(int i=0;i<root->son.size();i++) 
        if(root->son[i]->disabled==0)
    		deal(root->son[i]);
	lv--;
}
void error(){
	stringstream ss;
  	ss<<count;
	string x=string("error at step ")+ss.str()+string(" with ")+tokens_string[count];
	errors.push_back(x);
	errors_num+=1;
}
void GetSym(){
	count++;
	sym=tokens[count];
	current_node->addSon(tokens[count-1],tokens_string[count-1]);
	//cout<<"step "<<count<<" get "<<sym<<" "<<tokens_string[count]<<endl;
}
void keep(){
	count_keep.push_back(count);
	errors_num_keep.push_back(errors_num);
}
void load(){
	count=count_keep.back();
	sym=tokens[count];
	errors_num_keep.pop_back();
	count_keep.pop_back();
}
void load2(){
	count=count_keep.back();
	sym=tokens[count];
	for(int i=0;i<errors_num-errors_num_keep.back();i++) errors.pop_back();
	errors_num=errors_num_keep.back();
	errors_num_keep.pop_back();
	count_keep.pop_back();
}
void pop(){
	errors_num_keep.pop_back();
	count_keep.pop_back();
}
void parse_1();
void parse_2();
void parse_3();
void parse_4();
void parse_5();
void parse_6();
void parse_7();
void parse_8();
void parse_9();
void parse_10();
void parse_11();
void parse_12();
void parse_13();
void parse_14();
void parse_15();
void parse_17();
void parse_19();
void parse_20();
void parse_21();
void parse_22();
void parse_23();
void parse_24();
void parse_25();
void parse_1(){
	root_node->token=-1;
	root_node->token_string="program";
	current_node=root_node;
	parse_2();parse_6();
}
void parse_2(){
	keep();
	current_node=current_node->addSon(-1,"declarations");
	parse_3();
	if(sym==SEMI) GetSym();
	else error();
	node* del=current_node;
	current_node=current_node->father;
	if(errors_num>errors_num_keep.back()) {
	    load2();
	    del->disabled=1;
	}
	else{
		pop();
		parse_2();
	}
}
void parse_3(){
	keep();
	current_node=current_node->addSon(-1,"decl");
	parse_4();parse_5();
	node* del=current_node;
	current_node=current_node->father;
	if(errors_num>errors_num_keep.back()) {
	    load();
	    del->disabled=1;
	}
	else pop();
}
void parse_4(){
	keep();
	current_node=current_node->addSon(-1,"type-specifier");
	if(sym==INT||sym==BOOL||sym==CHAR) GetSym();
	else error();
	node* del=current_node;
	current_node=current_node->father;
	if(errors_num>errors_num_keep.back()){
	    load();
	    del->disabled=1;
	}
	else pop();
}
void parse_5(){
	keep();
	current_node=current_node->addSon(-1,"varlist");
	if(sym==ID) GetSym();
	while(sym==COMMA){
		GetSym();
		if(sym==ID) GetSym();
		else error();
	}
	node* del=current_node;
	current_node=current_node->father;
	if(errors_num>errors_num_keep.back()) {
	    load();
	    del->disabled=1;
	}
	else pop();
}
void parse_6(){
	keep();
	current_node=current_node->addSon(-1,"stmt-sequence");
	parse_7();
	while(sym==SEMI) {GetSym();parse_7();}
	node* del=current_node;
	current_node=current_node->father;
	if(errors_num>errors_num_keep.back()) {
	    load();
	    del->disabled=1;
	}
	else pop();
}
void parse_7(){
	keep();
	current_node=current_node->addSon(-1,"statement"); 
	switch(sym){
		case WHILE: parse_8();break;
		case IF: parse_9();break;
		case REPEAT: parse_10();break;
		case ID: parse_11();break;
		case READ: parse_12();break;
		case WRITE: parse_13();break;
		default: error();
	}
	node* del=current_node;
	current_node=current_node->father;
	if(errors_num>errors_num_keep.back()) {
	    load();
	    del->disabled=1;
	}
	else pop();
}
void parse_8(){
	keep();
	current_node=current_node->addSon(-1,"while-stmt");
	if(sym==WHILE) GetSym();
	else error();
	parse_20();
	if(sym==DO) GetSym();
	else error();
	parse_6();
	if(sym==END) GetSym();
	else error();
	node* del=current_node;
	current_node=current_node->father;
	if(errors_num>errors_num_keep.back()) {
	    load();
	    del->disabled=1;
	}
	else pop();
}
void parse_9(){
	keep();
	current_node=current_node->addSon(-1,"if-stmt");
	if(sym==IF) GetSym();
	else error();
	parse_20();
	if(sym==THEN) GetSym();
	else error();
	parse_6();
	if(sym==ELSE){
		GetSym();
		parse_6();
	}
	if(sym==END) GetSym();
	else error;
	node* del=current_node;
	current_node=current_node->father;
	if(errors_num>errors_num_keep.back()) {
	    load();
	    del->disabled=1;
	}
	else pop();
}
void parse_10(){
	keep();
	current_node=current_node->addSon(-1,"repeat-stmt");
	if(sym==REPEAT) GetSym();
	else error();
	parse_6();
	if(sym==UNTIL) GetSym();
	else error();
	parse_20();
	node* del=current_node;
	current_node=current_node->father;
	if(errors_num>errors_num_keep.back()) {
	    load();
	    del->disabled=1;
	}
	else pop();
}
void parse_11(){
	keep();
	current_node=current_node->addSon(-1,"assign-stmt");
	if(sym==ID) GetSym();
	else error();
	if(sym==ASSIGN) GetSym();
	else error();
	parse_14();
	node* del=current_node;
	current_node=current_node->father;
	if(errors_num>errors_num_keep.back()) {
	    load();
	    del->disabled=1;
	}
	else pop();
}
void parse_12(){
	keep();
	current_node=current_node->addSon(-1,"read-stmt");
	if(sym==READ) GetSym();
	else error();
	if(sym==ID) GetSym();
	else error();
	node* del=current_node;
	current_node=current_node->father;
	if(errors_num>errors_num_keep.back()) {
	    load();
	    del->disabled=1;
	}
	else pop();
}
void parse_13(){
	keep();
	current_node=current_node->addSon(-1,"write-stmt");
	if(sym==WRITE) GetSym();
	else error();
	parse_14();
	node* del=current_node;
	current_node=current_node->father;
	if(errors_num>errors_num_keep.back()) {
	    load();
	    del->disabled=1;
	}
	else pop();
}
void parse_14(){
	keep();
	current_node=current_node->addSon(-1,"exp");
	vector<void(*)()> all;
	all.push_back(parse_15);
	all.push_back(parse_20);
	all.push_back(parse_25);
	for(int i=0;i<all.size();i++){
		keep();
		all[i]();
		if(errors_num>errors_num_keep.back() && i!=all.size()-1) load2();	
		else{
			pop();
     		break;	
		}
	}
	node* del=current_node;
	current_node=current_node->father;
	if(errors_num>errors_num_keep.back()) {
	    load();
	    del->disabled=1;
	}
	else pop();
}
void parse_15(){
	keep();
	current_node=current_node->addSon(-1,"arithmetic-exp");
	parse_17();
	while(sym==PLUS||sym==MINUS) {GetSym();parse_17();}
	node* del=current_node;
	current_node=current_node->father;
	if(errors_num>errors_num_keep.back()) {
	    load();
	    del->disabled=1;
	}
	else pop();
}
void parse_17(){
	keep();
	current_node=current_node->addSon(-1,"term");
	parse_19();
	while(sym==OVER||sym==TIMES) {GetSym();parse_19();}
	node* del=current_node;
	current_node=current_node->father;
	if(errors_num>errors_num_keep.back()){
	    load();
	    del->disabled=1;
	}
	else pop();
}
void parse_19(){
	keep();
	current_node=current_node->addSon(-1,"factor");
	if(sym==NUM||sym==ID) GetSym();
	else{
		if(sym==LPAREN){
			GetSym();
			parse_15();
		}
		else error();
		if(sym==RPAREN) GetSym();
		else error();
	}
	node* del=current_node;
	current_node=current_node->father;
	if(errors_num>errors_num_keep.back()) {
	    load();
	    del->disabled=1;
	}
	else pop();
}
void parse_20(){
	keep();
	current_node=current_node->addSon(-1,"bool-exp");
	parse_21();
	while(sym==OR) {GetSym();parse_21();}
	node* del=current_node;
	current_node=current_node->father;
	if(errors_num>errors_num_keep.back()) {
	    load();
	    del->disabled=1;
	}
	else pop();
}
void parse_21(){
	keep();
	current_node=current_node->addSon(-1,"bterm");
	parse_22();
	while(sym==AND) {GetSym();parse_22();}
	node* del=current_node;
	current_node=current_node->father;
	if(errors_num>errors_num_keep.back()) {
	    load();
	    del->disabled=1;
	}
	else pop();
}
void parse_22(){
	keep();
	current_node=current_node->addSon(-1,"bfactor");
	parse_23();
	node* del=current_node;
	current_node=current_node->father;
	if(errors_num>errors_num_keep.back()) {
	    load();
	    del->disabled=1;
	}
	else pop();
}
void parse_23(){
	keep();
	current_node=current_node->addSon(-1,"comparision-exp");
	parse_15();parse_24();parse_15();
	node* del=current_node;
	current_node=current_node->father;
	if(errors_num>errors_num_keep.back()) {
	    load();
	    del->disabled=1;
	}
	else pop();
}
void parse_24(){
	keep();
	current_node=current_node->addSon(-1,"comparision-op");
	if(sym==EQ||sym==LT||sym==BT||sym==LET||sym==BET) GetSym();
	else error();
	node* del=current_node;
	current_node=current_node->father;
	if(errors_num>errors_num_keep.back()) {
	    load();
	    del->disabled=1;
	}
	else pop();
}
void parse_25(){
	keep();
	current_node=current_node->addSon(-1,"string-exp");
	if(sym==STR) GetSym();
	else error();
	node* del=current_node;
	current_node=current_node->father;
	if(errors_num>errors_num_keep.back()) {
	    load();
	    del->disabled=1;
	}
	else pop();
}
int main(){
	source=fopen(sfName,"r");
	while(getToken()!=ENDFILE);
    fclose(source);
    //for(int i=0;i<tokens.size();i++)
	//    cout<<i<<"   "<<tokens_string[i]<<"    "<<tokens[i]<<endl;
    sym=tokens[0];
    //cout<<"step 0"<<" get "<<sym<<endl;
    parse_1();
    //if(errors.size()==0) cout<<"no errors\n";
    //else{
	//	for(int i=0;i<errors.size();i++)
	//    	cout<<errors[i]<<endl;    	
	//}
	deal(root_node);
	for(int i=0;i<result.size();i++) cout<<result[i]<<endl;
	return 0;
}
