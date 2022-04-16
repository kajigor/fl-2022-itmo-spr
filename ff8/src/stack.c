#include <stdbool.h>
#include <stdio.h>
#include <malloc.h>
#include "../include/stack.h"

//������������� �����
void InitStack(struct Stack * st)
{
	st->buf = (char*)malloc(STACK_SIZE * sizeof(char));
	st->size = STACK_SIZE;
	st->hi = 0;
}

//����������
void PushStack(struct Stack *st, char symb)
{
	if (FullStack(st)) {
		ResizeStack(st);
	}
	st->buf[st->hi++] = symb;
}

//�������� �������
char TopStack(struct Stack * st)
{
	if (EmptyStack(st))
		return ERR;
	return st->buf[st->hi - 1];
}

//������� �������
void PopStack(struct Stack * st)
{
	if (!EmptyStack(st))
		st->hi--;
}

//�������� �� �������
bool FullStack(struct Stack * st)
{
	if (st->hi == st->size)
		return true;
	return false;
}

//�������� �� �������
bool EmptyStack(struct Stack * st)
{  
	if (st->hi==0)
		return true;
	return false;
}
//������� �����
void ClearStack(struct Stack * st)
{
	free(st->buf);
}

//�����������
void ResizeStack(struct Stack * st)
{
	int i;
	char* tmp = (char*)malloc((st->size + STACK_SIZE) * sizeof(char));
	for (i = 0; i < st->size; ++i)
		tmp[i] = st->buf[i];
	free(st->buf);
	st->buf = tmp;
	st->size += STACK_SIZE;
}

//�����
void PrintStack(struct Stack * st)
{
	int i;
	for (i = st->hi - 1; i >= 0; --i) {
		printf("%c", st->buf[i]);
	}
}


