#include <stdbool.h>
#include "str.h"

#define END '$' //������ ������ ������

//��������� ����������
struct Grammar {
	struct Str Vt; //��������� �������������� ��������
	struct Str Vn; //��������� ������������ ��������
	struct Rule {
		char a;
		struct Str b;
	}* P; //������� ������ ����������
	int S; //����� ���������� ������� ����������
	int size_P;
	struct Str* FIRST;
	struct Str* FOLLOW;
	int** table;
};

//������������� ����������
void InitGrammar(struct Grammar *g, int term_size, char* term, int state_size, char* state, char start);

//���������� �������
void AddRule(struct Grammar *g, char from, int size_to, char* to);

//����������� symb ��� ������������� �������
bool IsTerm(struct Grammar *g, char symb);

//����������� symb ��� ��������������� �������
bool IsState(struct Grammar *g, char symb);

//������� ���������� ��������� FIRST
void FindFIRST(struct Grammar *g);

//������� ���������� ��������� FOLLOW
void FindFOLLOW(struct Grammar *g);

//������� ���������� �������
bool CreateTable(struct Grammar *g);

//������� ������ � ��� ����
void WriteInFILE(struct Grammar *g);

//������
bool Parse(struct Grammar *g, int str_size, char* str);