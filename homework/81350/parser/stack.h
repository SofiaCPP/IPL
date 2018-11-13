#pragma once
#include <stdio.h>

#define MAXSIZE 1000
extern int stack[MAXSIZE];
extern int top;

int isempty();
int isfull();
int peek();
int pop();
int push(int);
