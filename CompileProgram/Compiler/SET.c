#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include "set.h"

symset uniteset(symset s1, symset s2) {//按elem大小从小到大合并s1和s2
	symset s;
	snode* p;

	s = p = (snode*)malloc(sizeof(snode));
	while (s1 && s2) {
		p->next = (snode*)malloc(sizeof(snode));
		p = p->next;
		if (s1->elem < s2->elem) {
			p->elem = s1->elem;
			s1 = s1->next;
		}
		else {
			p->elem = s2->elem;
			s2 = s2->next;
		}
	}

	while (s1) {
		p->next = (snode*)malloc(sizeof(snode));
		p = p->next;
		p->elem = s1->elem;
		s1 = s1->next;
	}

	while (s2) {
		p->next = (snode*)malloc(sizeof(snode));
		p = p->next;
		p->elem = s2->elem;
		s2 = s2->next;
	}

	p->next = NULL;

	return s;
} // uniteset

void setinsert(symset s, int elem) {//插入elem到s
	snode* p = s;
	snode* q;

	while (p->next && p->next->elem < elem) {
		p = p->next;
	}

	q = (snode*)malloc(sizeof(snode));
	q->elem = elem;
	q->next = p->next;
	p->next = q;
} // setinsert

//根据参数新建一个symset，注意链表头没有元素
symset createset(int elem, .../* SYM_NULL */) {
	va_list list;
	symset s;

	s = (snode*)malloc(sizeof(snode));
	s->next = NULL;

	va_start(list, elem);
	while (elem) {
		setinsert(s, elem);
		elem = va_arg(list, int);
	}
	va_end(list);
	return s;
} // createset

void destroyset(symset s) {//删除s
	snode* p;

	while (s) {
		p = s;
		s = s->next;
		free(p);
	}
} // destroyset

int inset(int elem, symset s) {//判断elem是否在s中
	s = s->next;
	while (s && s->elem < elem)
		s = s->next;

	if (s && s->elem == elem)
		return 1;
	else
		return 0;
} // inset
// EOF set.c