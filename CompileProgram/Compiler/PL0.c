// pl0 compiler source code

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "set.h"
#include "pl0.h"

//////////////////////////////////////////////////////////////////////
// print error message.
void error(int n) {
	int i;

	printf("      ");
	for (i = 1; i <= cc - 1; i++)
		printf(" ");
	printf("^\n");
	printf("Error %3d: %s\n", n, err_msg[n]);
	err++;
} // error

//////////////////////////////////////////////////////////////////////
//从源代码读入一行到缓冲line,每次从line中读取一个字符
//ll指line中的总字符数,cc指当前已读的字符数
void getch(void) {
	if (cc == ll) {//缓冲line消耗完,需再读入一行
		if (feof(infile)) {
			printf("\nPROGRAM INCOMPLETE\n");
			exit(1);
		}
		ll = cc = 0;
		printf("%5d  ", cx);
		while (!feof(infile) && (ch = getc(infile)) != '\n') {//读入一行
			printf("%c", ch);
			line[++ll] = ch;
		} // while
		printf("\n");
		line[++ll] = ' ';
	}
	ch = line[++cc];
} // getch

//////////////////////////////////////////////////////////////////////
// 从输入读入一个词法符号，词法分析器
void getsym1(void) {
	int i, k;
	char a[MAXIDLEN + 1];//a[11]

	while (ch == ' ')//忽略空白
		getch();

	if (isalpha(ch)) { //当前输入为字母,则输入词法符号应该为关键字或标识符
		k = 0;
		do {
			if (k < MAXIDLEN)
				a[k++] = ch;
			getch();
		} while (isalpha(ch) || isdigit(ch));//字母开头的字母数字组合
		a[k] = 0;//字符串结尾
		strcpy(id, a);

		//将已读取的id存于word[0],然后调用strcmp从word[11]到word[0]与id比较
		//如果id为关键字，则必有i>=0，否则因为word[0]=id，因此i=-1
		word[0] = id;
		i = NRW;//i=11
		while (strcmp(id, word[i--]));
		if (++i)
			sym = wsym[i]; // symbol is a reserved word
		else
			sym = SYM_IDENTIFIER;   // symbol is an identifier
	}
	else if (isdigit(ch)) { // symbol is a number.
		k = num = 0;
		sym = SYM_NUMBER;
		do { //识别数字
			num = num * 10 + ch - '0';
			k++;
			getch();
		} while (isdigit(ch));
		if (k > MAXNUMLEN)
			error(25);     // The number is too great.
	}
	else if (ch == ':') {
		getch();
		if (ch == '=') {
			sym = SYM_BECOMES; // :=
			getch();
		}
		else {
			sym = SYM_NULL;       // illegal?
		}
	}
	else if (ch == '>') {
		getch();
		if (ch == '=') {
			sym = SYM_GEQ;     // >=
			getch();
		}
		else {
			sym = SYM_GTR;     // >
		}
	}
	else if (ch == '<') {
		getch();
		if (ch == '=') {
			sym = SYM_LEQ;     // <=
			getch();
		}
		else if (ch == '>') {
			sym = SYM_NEQ;     // <>
			getch();
		}
		else {
			sym = SYM_LES;     // <
		}
	}
	else { // other tokens
	   //与关键字的比较类似
		i = NSYM; //i=10
		csym[0] = ch;
		while (csym[i--] != ch);
		if (++i) {
			sym = ssym[i];
			getch();
		}
		else {
			printf("Fatal Error: Unknown character.\n");
			exit(1);
		}
	}
} // getsym


//////////////////////////////////////////////////////////////////////
//* 使用DFA完成词法分析
void getsym(void) {
	int i, k;
	char a[MAXIDLEN + 1];//a[ll]
	while (ch == ' ')
		getch();
	int state;
	state = STA_START;
	i = 0;
	k = 0;
	num = 0;
	while (state != STA_DONE) {
		switch (state) {
		case STA_START:
			if (isalpha(ch)) {
				state = STA_IDorKEYWORD;
				a[k++] = ch;
				getch();
			}
			else if (isdigit(ch)) {
				num = num * 10 + ch - '0';
				state = STA_NUMBER;
				sym = SYM_NUMBER;
				getch();
			}
			else if (ch == ':') {
				state = STA_BECOMES;
				getch();

			}
			else if (ch == '>') {
				getch();
				state = STA_GEQorGTR;
			}
			else if (ch == '<') {
				state = STA_LEQorNEQorLES;
				getch();
			}
			else { // other tokens
			   //与关键字的比较类似
				state = STA_DONE;
				i = NSYM; //i=10
				csym[0] = ch;
				while (csym[i--] != ch);
				if (++i) {
					sym = ssym[i];
					getch();
				}
				else {
					printf("Fatal Error: Unknown character.\n");
					exit(1);
				}

			}
			break;

		case STA_IDorKEYWORD:
			if (isalpha(ch) || isdigit(ch)) {
				state = STA_IDorKEYWORD;
				if (k < MAXIDLEN)
					a[k++] = ch;
				getch();
			}
			else {
				a[k] = 0;//字符串结尾
				strcpy(id, a);

				word[0] = id;
				i = NRW;//i=11
				while (strcmp(id, word[i--]));		//相等返回0，1<2,返回负数
				if (++i)
					sym = wsym[i]; // symbol is a reserved word *关键字
				else
					sym = SYM_IDENTIFIER;   // symbol is an identifier *标识符
				state = STA_DONE;
			}
			break;
		case STA_NUMBER:
			if (isdigit(ch)) {
				state = STA_NUMBER;
				num = num * 10 + ch - '0';
				k++;
				getch();
			}
			else {
				state = STA_DONE;
				sym = SYM_NUMBER;
				if (k > MAXNUMLEN)
					error(25);
			}
			break;
		case STA_BECOMES:
			if (ch == '=') {
				sym = SYM_BECOMES; // :=
				getch();
			}
			else {
				sym = SYM_NULL;       // illegal?
			}
			state = STA_DONE;
			break;
		case STA_GEQorGTR:
			if (ch == '=') {
				sym = SYM_GEQ;     // >=
				getch();
			}
			else {
				sym = SYM_GTR;     // >
			}
			state = STA_DONE;
			break;
		case STA_LEQorNEQorLES:
			if (ch == '=') {
				sym = SYM_LEQ;     // <=
				getch();
			}
			else if (ch == '>') {
				sym = SYM_NEQ;     // <>
				getch();
			}
			else {
				sym = SYM_LES;     // <
			}
			state = STA_DONE;
			break;
		case STA_DONE:
			break;
		default:
			break;
		}
	}
}
//////////////////////////////////////////////////////////////////////
// generates (assembles) an instruction.
void gen(int x, int y, int z) {//生成汇编指令
	if (cx > CXMAX) {// cx > 500
		printf("Fatal Error: Program too long.\n");
		exit(1);
	}
	code[cx].f = x;
	code[cx].l = y;
	code[cx++].a = z;
} // gen

//////////////////////////////////////////////////////////////////////
// tests if error occurs and skips all symbols that do not belongs to s1 or s2.
void test(symset s1, symset s2, int n) {//参考第一部分2.6节
	symset s;

	if (!inset(sym, s1)) {
		error(n);
		s = uniteset(s1, s2);
		while (!inset(sym, s))
			getsym();
		destroyset(s);
	}
} // test

//////////////////////////////////////////////////////////////////////
int dx;  // data allocation index

// enter object(constant, variable or procedre) into table.
void enter(int kind) {//标识符填入符号表
	mask* mk;

	tx++;
	strcpy(table[tx].name, id);
	table[tx].kind = kind;
	switch (kind) {
	case ID_CONSTANT:
		if (num > MAXADDRESS) {//常数的大小不能超过32767
			error(25); // The number is too great.
			num = 0;
		}
		table[tx].value = num;
		break;
	case ID_VARIABLE:
		mk = (mask*)&table[tx];
		mk->level = level;
		mk->address = dx++;
		break;
	case ID_PROCEDURE:
		mk = (mask*)&table[tx];
		mk->level = level;
		break;
	} // switch
} // enter

//////////////////////////////////////////////////////////////////////
// locates identifier in symbol table.
int position(char* id) {//在符号表table中查找id
	int i;
	strcpy(table[0].name, id);
	i = tx + 1;
	while (strcmp(table[--i].name, id) != 0);
	return i;
} // position

//////////////////////////////////////////////////////////////////////
void constdeclaration() {//常量填入符号表
	if (sym == SYM_IDENTIFIER) {//全局变量id中存有已识别的标识符
		getsym();
		if (sym == SYM_EQU || sym == SYM_BECOMES) {
			if (sym == SYM_BECOMES)
				error(1); // Found ':=' when expecting '='.
			getsym();
			if (sym == SYM_NUMBER) {//全局变量num中存有已识别的数字
				enter(ID_CONSTANT);
				getsym();
			}
			else {
				error(2); // There must be a number to follow '='.
			}
		}
		else {
			error(3); // There must be an '=' to follow the identifier.
		}
	}
	else {
		error(4); // There must be an identifier to follow 'const'.
	}
} // constdeclaration

//////////////////////////////////////////////////////////////////////
void vardeclaration(void) { //变量填入符号表
	if (sym == SYM_IDENTIFIER) {
		enter(ID_VARIABLE);
		getsym();
	}
	else {
		error(4); // There must be an identifier to follow 'var'.
	}
} // vardeclaration

//////////////////////////////////////////////////////////////////////
void listcode(int from, int to) {
	int i;

	printf("\n");
	for (i = from; i < to; i++) {
		printf("%5d %s\t%d\t%d\n", i, mnemonic[code[i].f], code[i].l, code[i].a);
	}
	printf("\n");
} // listcode

//////////////////////////////////////////////////////////////////////
void factor(symset fsys) {
	void expression();
	int i;
	symset set;

	test(facbegsys, fsys, 24); // The symbol cannot be as the beginning of an expression.

	while (inset(sym, facbegsys)) {
		if (sym == SYM_IDENTIFIER) {
			if ((i = position(id)) == 0) {
				error(11); // Undeclared identifier.
			}
			else {
				switch (table[i].kind) {
					mask* mk;
				case ID_CONSTANT:
					gen(LIT, 0, table[i].value);
					break;
				case ID_VARIABLE:
					mk = (mask*)&table[i];
					gen(LOD, level - mk->level, mk->address);
					break;
				case ID_PROCEDURE:
					error(21); // Procedure identifier cannot be in an expression.
					break;
				} // switch
			}
			getsym();
		}
		else if (sym == SYM_NUMBER) {
			if (num > MAXADDRESS) {
				error(25); // The number is too great.
				num = 0;
			}
			gen(LIT, 0, num);
			getsym();
		}
		else if (sym == SYM_LPAREN) {
			getsym();
			set = uniteset(createset(SYM_RPAREN, SYM_NULL), fsys);
			expression(set);
			destroyset(set);
			if (sym == SYM_RPAREN) {
				getsym();
			}
			else {
				error(22); // Missing ')'.
			}
		}
		test(fsys, createset(SYM_LPAREN, SYM_NULL), 23);
	} // while
} // factor

//////////////////////////////////////////////////////////////////////
void term(symset fsys) {
	int mulop;
	symset set;

	set = uniteset(fsys, createset(SYM_TIMES, SYM_SLASH, SYM_NULL));
	factor(set);
	while (sym == SYM_TIMES || sym == SYM_SLASH) {
		mulop = sym;
		getsym();
		factor(set);
		if (mulop == SYM_TIMES)
		{
			gen(OPR, 0, OPR_MUL);
		}
		else
		{
			gen(OPR, 0, OPR_DIV);
		}
	} // while
	destroyset(set);
} // term

//////////////////////////////////////////////////////////////////////
void expression(symset fsys) { //递归下降法中对<表达式>的处理
	int addop;
	symset set;

	set = uniteset(fsys, createset(SYM_PLUS, SYM_MINUS, SYM_NULL));
	if (sym == SYM_PLUS || sym == SYM_MINUS) {
		addop = sym;
		getsym();
		term(set);
		if (addop == SYM_MINUS) {
			gen(OPR, 0, OPR_NEG);
		}
	}
	else {
		term(set);
	}

	while (sym == SYM_PLUS || sym == SYM_MINUS) {
		addop = sym;
		getsym();
		term(set);
		if (addop == SYM_PLUS) {
			gen(OPR, 0, OPR_ADD);
		}
		else {
			gen(OPR, 0, OPR_MIN);
		}
	} // while

	destroyset(set);
} // expression

//////////////////////////////////////////////////////////////////////
void condition(symset fsys) { //递归下降法中对<条件>的处理
	int relop;
	symset set;

	if (sym == SYM_ODD) {
		getsym();
		expression(fsys);
		gen(OPR, 0, 6);
	}
	else {
		set = uniteset(relset, fsys);
		expression(set);
		destroyset(set);
		if (!inset(sym, relset)) {
			error(20);
		}
		else {
			relop = sym;
			getsym();
			expression(fsys);
			switch (relop) {//根据比较关系生成指令
			case SYM_EQU:
				gen(OPR, 0, OPR_EQU);
				break;
			case SYM_NEQ:
				gen(OPR, 0, OPR_NEQ);
				break;
			case SYM_LES:
				gen(OPR, 0, OPR_LES);
				break;
			case SYM_GEQ:
				gen(OPR, 0, OPR_GEQ);
				break;
			case SYM_GTR:
				gen(OPR, 0, OPR_GTR);
				break;
			case SYM_LEQ:
				gen(OPR, 0, OPR_LEQ);
				break;
			} // switch
		} // else
	} // else
} // condition

//////////////////////////////////////////////////////////////////////
void statement(symset fsys) { //递归下降法中对<语句>的处理
	int i, cx1, cx2;
	symset set1, set;

	if (sym == SYM_IDENTIFIER) { // variable assignment
		mask* mk;
		if (!(i = position(id))) {
			error(11); // Undeclared identifier.
		}
		else if (table[i].kind != ID_VARIABLE) {
			error(12); // Illegal assignment.
			i = 0;
		}
		getsym();
		if (sym == SYM_BECOMES) {
			getsym();
		}
		else {
			error(13); // ':=' expected.
		}
		expression(fsys);
		mk = (mask*)&table[i];
		if (i) {
			gen(STO, level - mk->level, mk->address);
		}
	}
	else if (sym == SYM_CALL) { // procedure call
		getsym();
		if (sym != SYM_IDENTIFIER) {
			error(14); // There must be an identifier to follow the 'call'.
		}
		else {
			if (!(i = position(id))) {
				error(11); // Undeclared identifier.
			}
			else if (table[i].kind == ID_PROCEDURE) {
				mask* mk;
				mk = (mask*)&table[i];
				gen(CAL, level - mk->level, mk->address);
			}
			else {
				error(15); // A constant or variable can not be called. 
			}
			getsym();
		}
	}
	else if (sym == SYM_IF) { // if statement
		getsym();
		set1 = createset(SYM_THEN, SYM_DO, SYM_NULL);
		set = uniteset(set1, fsys);
		condition(set);
		destroyset(set1);
		destroyset(set);
		if (sym == SYM_THEN) {
			getsym();
		}
		else {
			error(16); // 'then' expected.
		}
		cx1 = cx;
		gen(JPC, 0, 0);
		statement(fsys);
		code[cx1].a = cx;
	}
	else if (sym == SYM_BEGIN) { // block
		getsym();
		set1 = createset(SYM_SEMICOLON, SYM_END, SYM_NULL);
		set = uniteset(set1, fsys);
		statement(set);//该行代码和后续的while循环处理语句序列
		while (sym == SYM_SEMICOLON || inset(sym, statbegsys)) {
			if (sym == SYM_SEMICOLON) {
				getsym();
			}
			else {
				error(10);
			}
			statement(set);
		} // while
		destroyset(set1);
		destroyset(set);
		if (sym == SYM_END) {
			getsym();
		}
		else {
			error(17); // ';' or 'end' expected.
		}
	}
	else if (sym == SYM_WHILE) { // while statement
		cx1 = cx;
		getsym();
		set1 = createset(SYM_DO, SYM_NULL);
		set = uniteset(set1, fsys);
		condition(set);
		destroyset(set1);
		destroyset(set);
		cx2 = cx;
		gen(JPC, 0, 0);
		if (sym == SYM_DO) {
			getsym();
		}
		else {
			error(18); // 'do' expected.
		}
		statement(fsys);
		gen(JMP, 0, cx1);
		code[cx2].a = cx;
	}
	test(fsys, phi, 19);
} // statement

//////////////////////////////////////////////////////////////////////
void block(symset fsys) {//递归下降法中对<程序体>的处理
	int cx0; // initial code index

	//后续变量定义主要用于代码生成
	mask* mk;
	int block_dx;
	int savedTx;
	symset set1, set;
	dx = 3;
	mk = (mask*)&table[tx];
	mk->address = cx;
	gen(JMP, 0, 0);
	if (level > MAXLEVEL) {
		error(32); // There are too many levels.
	}

	//语法分析开始
	do {
		if (sym == SYM_CONST) { // constant declarations
			getsym();
			do {//循环处理id1=num1,id2=num2,……
				constdeclaration();
				while (sym == SYM_COMMA) {
					getsym();
					constdeclaration();
				}
				//常量定义结束
				if (sym == SYM_SEMICOLON) {
					getsym();
				}
				else {
					error(5); // Missing ',' or ';'.
				}
			} while (sym == SYM_IDENTIFIER);
		} // if

		if (sym == SYM_VAR) { // variable declarations
			getsym();
			do {
				vardeclaration();
				while (sym == SYM_COMMA) {
					getsym();
					vardeclaration();
				}
				if (sym == SYM_SEMICOLON) {
					getsym();
				}
				else {
					error(5); // Missing ',' or ';'.
				}
			} while (sym == SYM_IDENTIFIER);
			//			block = dx;
		} // if

		while (sym == SYM_PROCEDURE) { // procedure declarations
			getsym();
			if (sym == SYM_IDENTIFIER) {
				enter(ID_PROCEDURE);
				getsym();
			}
			else {
				error(4); // There must be an identifier to follow 'procedure'.
			}

			if (sym == SYM_SEMICOLON) {
				getsym();
			}
			else {
				error(5); // Missing ',' or ';'.
			}

			level++;//函数可以嵌套,level是函数的嵌套级别
			savedTx = tx;
			block_dx = dx;
			set1 = createset(SYM_SEMICOLON, SYM_NULL);
			set = uniteset(set1, fsys);
			block(set);//调用函数定义中的<程序体>处理函数block
			destroyset(set1);
			destroyset(set);
			tx = savedTx;
			level--;
			dx = block_dx;

			if (sym == SYM_SEMICOLON) {
				getsym();
				set1 = createset(SYM_IDENTIFIER, SYM_PROCEDURE, SYM_NULL);
				set = uniteset(statbegsys, set1);
				test(set, fsys, 6);
				destroyset(set1);
				destroyset(set);
			}
			else {
				error(5); // Missing ',' or ';'.
			}
		} // while
		set1 = createset(SYM_IDENTIFIER, SYM_NULL);
		set = uniteset(statbegsys, set1);
		test(set, declbegsys, 7);
		destroyset(set1);
		destroyset(set);
	} while (inset(sym, declbegsys));

	// 后续部分主要用于代码生成
	code[mk->address].a = cx;
	mk->address = cx;
	cx0 = cx;
	gen(INT, 0, dx);
	set1 = createset(SYM_SEMICOLON, SYM_END, SYM_NULL);
	set = uniteset(set1, fsys);
	statement(set);
	destroyset(set1);
	destroyset(set);
	gen(OPR, 0, OPR_RET); // return
	test(fsys, phi, 8); // test for error: Follow the statement is an incorrect symbol.
	listcode(cx0, cx);
} // block

//////////////////////////////////////////////////////////////////////
int base(int stack[], int currentLevel, int levelDiff) {
	int b = currentLevel;

	while (levelDiff--)
		b = stack[b];
	return b;
} // base

//////////////////////////////////////////////////////////////////////
// interprets and executes codes.
void interpret() {
	int pc;        // program counter
	int stack[STACKSIZE];
	int top;       // top of stack
	int b;         // program, base, and top-stack register
	instruction i; // instruction register

	printf("Begin executing PL/0 program.\n");

	pc = 0;
	b = 1;
	top = 3;
	stack[1] = stack[2] = stack[3] = 0;
	do {
		i = code[pc++];
		switch (i.f) {
		case LIT:
			stack[++top] = i.a;
			break;
		case OPR:
			switch (i.a) // operator
			{
			case OPR_RET:
				top = b - 1;
				pc = stack[top + 3];
				b = stack[top + 2];
				break;
			case OPR_NEG:
				stack[top] = -stack[top];
				break;
			case OPR_ADD:
				top--;
				stack[top] += stack[top + 1];
				break;
			case OPR_MIN:
				top--;
				stack[top] -= stack[top + 1];
				break;
			case OPR_MUL:
				top--;
				stack[top] *= stack[top + 1];
				break;
			case OPR_DIV:
				top--;
				if (stack[top + 1] == 0) {
					fprintf(stderr, "Runtime Error: Divided by zero.\n");
					fprintf(stderr, "Program terminated.\n");
					continue;
				}
				stack[top] /= stack[top + 1];
				break;
			case OPR_ODD:
				stack[top] %= 2;
				break;
			case OPR_EQU:
				top--;
				stack[top] = stack[top] == stack[top + 1];
				break;
			case OPR_NEQ:
				top--;
				stack[top] = stack[top] != stack[top + 1];
			case OPR_LES:
				top--;
				stack[top] = stack[top] < stack[top + 1];
				break;
			case OPR_GEQ:
				top--;
				stack[top] = stack[top] >= stack[top + 1];
			case OPR_GTR:
				top--;
				stack[top] = stack[top] > stack[top + 1];
				break;
			case OPR_LEQ:
				top--;
				stack[top] = stack[top] <= stack[top + 1];
			} // switch
			break;
		case LOD:
			stack[++top] = stack[base(stack, b, i.l) + i.a];
			break;
		case STO:
			stack[base(stack, b, i.l) + i.a] = stack[top];
			printf("%d\n", stack[top]);
			top--;
			break;
		case CAL:
			stack[top + 1] = base(stack, b, i.l);
			// generate new block mark
			stack[top + 2] = b;
			stack[top + 3] = pc;
			b = top + 1;
			pc = i.a;
			break;
		case INT:
			top += i.a;
			break;
		case JMP:
			pc = i.a;
			break;
		case JPC:
			if (stack[top] == 0)
				pc = i.a;
			top--;
			break;
		} // switch
	} while (pc);

	printf("End executing PL/0 program.\n");
} // interpret

//////////////////////////////////////////////////////////////////////
void main() {
	FILE* hbin;
	char s[80];
	int i;
	symset set, set1, set2;

	printf("Please input source file name: "); // get file name to be compiled
	scanf("%s", s);
	if ((infile = fopen(s, "r")) == NULL) {
		printf("File %s can't be opened.\n", s);
		exit(1);
	}

	phi = createset(SYM_NULL);
	relset = createset(SYM_EQU, SYM_NEQ, SYM_LES, SYM_LEQ, SYM_GTR, SYM_GEQ, SYM_NULL);

	// create begin symbol sets
	declbegsys = createset(SYM_CONST, SYM_VAR, SYM_PROCEDURE, SYM_NULL);
	statbegsys = createset(SYM_BEGIN, SYM_CALL, SYM_IF, SYM_WHILE, SYM_NULL);
	facbegsys = createset(SYM_IDENTIFIER, SYM_NUMBER, SYM_LPAREN, SYM_NULL);

	err = cc = cx = ll = 0; // initialize global variables
	ch = ' ';
	kk = MAXIDLEN; // kk=10

	getsym();

	set1 = createset(SYM_PERIOD, SYM_NULL);
	set2 = uniteset(declbegsys, statbegsys);
	set = uniteset(set1, set2);
	block(set);
	destroyset(set1);
	destroyset(set2);
	destroyset(set);
	destroyset(phi);
	destroyset(relset);
	destroyset(declbegsys);
	destroyset(statbegsys);
	destroyset(facbegsys);

	system("pause");

	// 后续代码用于解释执行中间代码
	/*
		if (sym != SYM_PERIOD)
			error(9); // '.' expected.
		if (err == 0) {
			hbin = fopen("hbin.txt", "w");
			for (i = 0; i < cx; i++)
				fwrite(&code[i], sizeof(instruction), 1, hbin);
			fclose(hbin);
		}
		if (err == 0)
			interpret();
		else
			printf("There are %d error(s) in PL/0 program.\n", err);
		listcode(0, cx);
	*/
} // main    END OF PL0.c
