# Makefile for TicTacToe 

CC = gfortran
CFLAGS = -fall-intrinsics -std=f95 -Wall

build:
	$(CC) $(CFLAGS) tictactoe.f95 -o tictactoe