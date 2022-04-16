#define _CRT_SECURE_NO_WARNINGS

#include <stdlib.h>
#include <stdio.h>
#include "../include/grammar.h"
#include "../include/stack.h"

//������������� ����������
void InitGrammar(struct Grammar *g, int term_size, char* term, int state_size, char* state, char start) {
	int i;
	//���������� ����� ������������ ��������
	InitStr(&g->Vt, term_size, term);
	//���������� ����� �������������� ��������
	InitStr(&g->Vn, state_size, state);
	//��������� ������������� ��������� ������
	g->size_P = 0;
	g->P = NULL;
	//��������� ���������� �����������
	for(i=0; i<g->Vn.size; ++i)
		if (start==g->Vn.s[i])
			g->S = i;
}

//���������� �������
void AddRule(struct Grammar *g, char from, int size_to, char * to)
{
	int i; //���������� ��� ������
	//�������� ���������� ������ ��� �������� ��������
	struct Rule* tmp = (struct Rule*)malloc((g->size_P + 1) * sizeof(struct Rule));
	for (i = 0; i < g->size_P; ++i)
		tmp[i] = g->P[i];
	tmp[g->size_P].a = from;
	InitStr(&tmp[g->size_P].b, size_to, to);
	//������� ������ (���� ���������) � ������
	if (g->size_P)
		free(g->P);
	g->P = tmp;
	//���������� ����� ������
	g->size_P++;
}

//����������� symb ��� ������������� �������
bool IsTerm(struct Grammar *g, char symb)
{
	return FindChar(&g->Vt, symb)!=-1;
}

//����������� symb ��� ��������������� �������
bool IsState(struct Grammar *g, char symb)
{
	return FindChar(&g->Vn, symb)!=-1;
}

//������� ���������� ��������� FIRST
void FindFIRST(struct Grammar *g)
{
	struct Str* old_FIRST; //FIRSTi �� ���������
	struct Str* new_FIRST; //FIRSTi+1 �� ���������
	int i;
	int j;
	bool check = true; //���������� ��� �������� ����������
	//��������� ������ ��� ����� FIRST
	g->FIRST = (struct Str*)malloc(g->Vn.size * sizeof(struct Str));
	for (i = 0; i < g->Vn.size; ++i) {
		InitStr(&g->FIRST[i], 0, NULL);
	}
	//��������� ������ ��� ����� old_FIRST
	old_FIRST = (struct Str*)malloc(g->Vn.size * sizeof(struct Str));
	for (i = 0; i < g->Vn.size; ++i) {
		InitStr(&old_FIRST[i], 0, NULL);
	}
	//��������� ������ ��� ����� new_FIRST
	new_FIRST = (struct Str*)malloc(g->Vn.size * sizeof(struct Str));
	for (i = 0; i < g->Vn.size; ++i) {
		InitStr(&new_FIRST[i], 0, NULL);
	}
	//���������� old_FIRST ��� FIRST0 �� ��������� (��� 1)
	for (i = 0; i < g->Vn.size; ++i) {
		for (j = 0; j < g->size_P; ++j) {
			if (g->Vn.s[i] == g->P[j].a)
				AddChar(&old_FIRST[i], g->P[j].b.s[0]);
		}
	}
	//������ �������������� ��������
	while (check) {
		//������� � new_FIRST ��������� �� �������������� �������� �� ���������(��� 2)
		for (i = 0; i < g->Vn.size; ++i) {
			CopyStr(&new_FIRST[i], old_FIRST[i].size, old_FIRST[i].s);
			for (j = 0; j < g->Vn.size; ++j) {
				if (FindChar(&old_FIRST[i], g->Vn.s[j])!=-1)
					AddStr(&new_FIRST[i], &old_FIRST[j]);
			}
		}
		//�������� �� ���������� (��� 3)
		check = false;
		for (i = 0; i < g->Vn.size; ++i) {
			if (!CompareStr(&old_FIRST[i], &new_FIRST[i]))
				check = true;
		}
		for (i = 0; i < g->Vn.size; ++i)
			CopyStr(&old_FIRST[i], new_FIRST[i].size, new_FIRST[i].s);
	}
	//��������� ������ � FIRST ��� �������������� �������� (��� 4) � ������� ������ ������
	for (i = 0; i < g->Vn.size; ++i) {
		CopyStr(&g->FIRST[i], old_FIRST[i].size, old_FIRST[i].s);
		for (j = 0; j < g->Vn.size; ++j)
			DelChar(&g->FIRST[i], g->Vn.s[j]);
		ClearStr(&new_FIRST[i]);
		ClearStr(&old_FIRST[i]);
	}
	//��������� ������
	free(new_FIRST);
	free(old_FIRST);
}

//������� ���������� ��������� FOLLOW
void FindFOLLOW(struct Grammar * g)
{
	struct Str* old_FOLLOW; //����� FOLLOWi
	struct Str* mid_FOLLOW; //������ FOLLOWi'
	struct Str* new_FOLLOW; //������ FOLLOWi+1
	int i;
	int j;
	int k;
	bool check=true; ////���������� ��� �������� ����������
	//��������� ������ � ��������� ������������� ��������
	g->FOLLOW = (struct Str*)malloc(g->Vn.size * sizeof(struct Str));
	for (i = 0; i < g->Vn.size; ++i)
		InitStr(&g->FOLLOW[i], 0, NULL);
	old_FOLLOW = (struct Str*)malloc(g->Vn.size * sizeof(struct Str));
	for (i = 0; i < g->Vn.size; ++i)
		InitStr(&old_FOLLOW[i], 0, NULL);
	mid_FOLLOW = (struct Str*)malloc(g->Vn.size * sizeof(struct Str));
	for (i = 0; i < g->Vn.size; ++i)
		InitStr(&mid_FOLLOW[i], 0, NULL);
	new_FOLLOW = (struct Str*)malloc(g->Vn.size * sizeof(struct Str));
	for (i = 0; i < g->Vn.size; ++i)
		InitStr(&new_FOLLOW[i], 0, NULL);
	//���������� FOLLOW0 (��� 1)
	for (i = 0; i < g->Vn.size; ++i) {
		for (j = 0; j < g->size_P; ++j) {
			for (k = 0; k < g->P[j].b.size-1; ++k) {
				if (g->Vn.s[i] == g->P[j].b.s[k])
					AddChar(&old_FOLLOW[i], g->P[j].b.s[k + 1]);
			}
		}
	}
	//���������� ������� ����� (��� 2)
	AddChar(&old_FOLLOW[g->S], END);
	while (check) {
        //������� �������� �� FIRST � FOLLOW (���� 3 � 4)
		for (i = 0; i < g->Vn.size; ++i) {
			CopyStr(&mid_FOLLOW[i], old_FOLLOW[i].size, old_FOLLOW[i].s);
			for (j = 0; j < g->Vn.size; ++j) {
				if (FindChar(&old_FOLLOW[i], g->Vn.s[j])!=-1) {
					AddStr(&mid_FOLLOW[i], &g->FIRST[j]);
					for(k=0; k<g->size_P; ++k) {
                        if (g->P[k].a==g->Vn.s[j] && g->P[k].b.s[0]==END)
                            AddStr(&mid_FOLLOW[i], &old_FOLLOW[j]);
					}
				}
			}
		}
		//  ����������� FOLLOW �� ���������� (��� 5)
        for (i = 0; i < g->Vn.size; ++i) {
			CopyStr(&new_FOLLOW[i], mid_FOLLOW[i].size, mid_FOLLOW[i].s);
			for (j = 0; j < g->size_P; ++j) {
                if(g->Vn.s[i]==g->P[j].b.s[g->P[j].b.size-1]) {
                    k=FindChar(&g->Vn, g->P[j].a);
                    AddStr(&new_FOLLOW[i], &mid_FOLLOW[k]);
                }
			}
		}
		//�������� �� ���������� (��� 6)
		check = false;
		for (i = 0; i < g->Vn.size; ++i) {
			if (!CompareStr(&old_FOLLOW[i], &new_FOLLOW[i]))
				check = true;
		}
		for (i = 0; i < g->Vn.size; ++i)
			CopyStr(&old_FOLLOW[i], new_FOLLOW[i].size, new_FOLLOW[i].s);
	}
	//���������� �������� � FOLLOW � �������� ������������ (��� 7)
	for (i = 0; i < g->Vn.size; ++i) {
		CopyStr(&g->FOLLOW[i], old_FOLLOW[i].size, old_FOLLOW[i].s);
		for (j = 0; j < g->Vn.size; ++j)
			DelChar(&g->FOLLOW[i], g->Vn.s[j]);
		ClearStr(&new_FOLLOW[i]);
		ClearStr(&mid_FOLLOW[i]);
		ClearStr(&old_FOLLOW[i]);
	}
	//��������� ������
	free(new_FOLLOW);
	free(mid_FOLLOW);
	free(old_FOLLOW);
}

//������� ���������� �������
bool CreateTable(struct Grammar *g) {
	//���������� ��� ������ � ����������
    int i, j;
	int x, y, k;
	//�������� ������ � ��������� ��� �������� (-1)
    g->table=(int**)malloc(g->Vn.size*sizeof(int*));
    for(x=0; x<g->Vn.size; ++x) {
        g->table[x]=(int*)malloc((g->Vt.size+1)*sizeof(int));
        for(y=0; y<g->Vt.size; ++y)
            g->table[x][y]=-1;
    }
	//��������� �������� ���������
    for(i=0; i<g->size_P; ++i) {
        x=FindChar(&g->Vn, g->P[i].a);
		//���� ������������ ������ - ��������� ������� � ��������������� ����
        if(IsTerm(g, g->P[i].b.s[0])) {
            y=FindChar(&g->Vt, g->P[i].b.s[0]);
            if (g->table[x][y] != -1 && g->table[x][y]  != i)
                return false;
            g->table[x][y]=i;

			if (g->Vt.s[y] == END) {
				for (y = 0; y<g->Vt.size; ++y) {
					if (FindChar(&g->FOLLOW[x], g->Vt.s[y]) != -1) {
                        if (g->table[x][y] != -1 && g->table[x][y]  != i)
                            return false;
                        g->table[x][y]=i;
                    }
				}
			}
        }
		//���� �������������� ������
        else if(IsState(g, g->P[i].b.s[0])) {
            j=FindChar(&g->Vn, g->P[i].b.s[0]);
			//��������� ��� ��������� �� FIRST ����� ����������� (��� 1)
            for(y=0; y<g->Vt.size; ++y) {
                if (FindChar(&g->FIRST[j], g->Vt.s[y])!=-1) {
                    if (g->table[x][y] != -1 && g->table[x][y]  != i)
                        return false;
                    g->table[x][y]=i;
                }
            }
			//���� FIRST ����������� �������� ������ �����, �� ��������� ��������� �� FOLLOW ����������� (��� 2)
            if (FindChar(&g->FIRST[j], END)!=-1) {
                for(y=0; y<g->Vt.size; ++y) {
                    if (FindChar(&g->FOLLOW[x], g->Vt.s[y])!=-1) {
                        if (g->table[x][y] != -1 && g->table[x][y]  != i)
                            return false;
                        g->table[x][y]=i;
                    }
                }
            }
        }
    }
    return true;
}

//������
bool Parse(struct Grammar *g, int str_size, char* str) {
    bool err=false;
    char ch;
    int pos;
    int num_rule;
    int i;
	struct Stack st;
    pos=0;
	str[str_size - 1] = END;
	InitStack(&st);
    PushStack(&st, END);
    PushStack(&st, g->Vn.s[g->S]);
    PrintStack(&st);
	str_size--;
    while(!err) {
        ch=TopStack(&st);
        if (IsState(g, ch)) {
            PopStack(&st);
			if (FindChar(&g->Vt, str[pos]) != -1)
				num_rule = g->table[FindChar(&g->Vn, ch)][FindChar(&g->Vt, str[pos])];
			else
				num_rule = -1;
			if (num_rule == -1) {
				printf(" -> ");
				printf("ERROR_NOT_GRAMMAR");
				err = true;
				break;
			}
			for (i = g->P[num_rule].b.size - 1; i >= 0; --i) {
				if (g->P[num_rule].b.s[i]!=END)
					PushStack(&st, g->P[num_rule].b.s[i]);
			}
			printf(" -> ");
			for (i = 0; i<pos; ++i)
				printf("%c", str[i]);
			PrintStack(&st);
        }
		else if (ch == END && pos == str_size) {
			printf(" -> ");
			for (i = 0; i<pos; ++i)
				printf("%c", str[i]);
			break;
		}
        else if (IsTerm(g, ch) && ch==str[pos]) {
            pos++;
            PopStack(&st);
        }
		else {
			printf(" -> ");
			printf("ERROR_NOT_GRAMMAR");
			err = true;
		}
    }
    return !err;
}

//������� ������ � ��� ����
void WriteInFILE(struct Grammar *g) {
    FILE* f;
    int i, j, k;
    int rule;
	//������ FIRST
    f=fopen("FIRST.bin", "w");
    fprintf(f, "\tFIRST\n");
    for(i=0; i<g->Vn.size; ++i) {
        fprintf(f, "%c\t", g->Vn.s[i]);
        for(j=0; j<g->FIRST[i].size; ++j)
            fprintf(f, "%c", g->FIRST[i].s[j]);
        fprintf(f, "\n");
    }
	fclose(f);
	//������ FOLLOW
    f=fopen("FOLLOW.bin", "w");
    fprintf(f, "\tFOLLOW\n");
    for(i=0; i<g->Vn.size; ++i) {
        fprintf(f, "%c\t", g->Vn.s[i]);
        for(j=0; j<g->FOLLOW[i].size; ++j)
            fprintf(f, "%c", g->FOLLOW[i].s[j]);
        fprintf(f, "\n");
    }
   fclose(f);
   //������ �������
    f=fopen("TABLE.bin", "w");
    for(j=0; j<g->Vt.size; ++j)
        fprintf(f, "\t%c", g->Vt.s[j]);
    fprintf(f, "\n");
    for(i=0; i<g->Vn.size; ++i) {
        fprintf(f, "%c", g->Vn.s[i]);
        for(j=0; j<g->Vt.size; ++j) {
            fprintf(f, "\t");
            rule=g->table[i][j];
            if (rule==-1)
                fprintf(f, "Err");
            else {
				fprintf(f, "%c->", g->P[rule].a);
				for(k=0; k<g->P[rule].b.size; ++k)
					fprintf(f, "%c", g->P[rule].b.s[k]);
            }
        }
        fprintf(f, "\n");
    }
    fclose(f);
}

