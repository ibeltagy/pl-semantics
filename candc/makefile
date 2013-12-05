# -*- Mode: Makefile; -*-
# C&C NLP tools
# Copyright (c) Universities of Edinburgh, Oxford and Sydney
# Copyright (c) James R. Curran
#
# This software is covered by a non-commercial use licence.
# See LICENCE.txt for the full text of the licence.
#
# If LICENCE.txt is not included in this distribution
# please email candc@it.usyd.edu.au to obtain a copy.

PORT = unix
PORT_OBJS = src/lib/ports/unix.o src/lib/ports/unix_common.o src/lib/ports/colour.o

CC = gcc
CFLAGS = -Wall -O3 $(INC)

CXX = g++
CXXFLAGS = $(CFLAGS)

LD=$(CXX)
LDFLAGS = 
DEPEND = $(CC) $(INC)

#INC = -I/usr/other/ask/include -Isrc/include
#-I/home/cluster/mnt/include/boost-1_39 -I/home/cluster/mnt/include
INC = -Isrc/include -Iext/include
#LIBS = -Llib -lm -L/home/cluster/mnt/lib
#LIBS = -L/home/cluster/mnt/lib -lboost_mpi-gcc41-mt -lmrmpi
LIBS = -Llib -lm -Lext/lib

SOAP_INC = $(INC)
SOAP_LIBS = $(LIBS) -lgsoap++
SOAP_CPP = ext/bin/soapcpp2
SOAP_CXXFLAGS = $(CXXFLAGS) $(SOAP_INC)
SOAP_LDFLAGS = $(LDFLAGS)

PYTHON = python2.6

SHAREDLIB = so
LIB_LDFLAGS = -shared $(LDFLAGS)

PY_INC = -Iext/include -Iext/include/$(PYTHON)
PY_LIBS = $(LIBS) -lcandc -Lext/lib -lboost_python
PY_CXXFLAGS = $(CXXFLAGS) -fPIC $(PY_INC)
PY_LDFLAGS = -shared $(LDFLAGS)

MPIXX = mpicxx
MPI_CXXFLAGS = $(CXXFLAGS) -DMPICH_IGNORE_CXX_SEEK -Iext/include/mrmpi
MPI_LIBS = $(LIBS) -lboost_mpi -lboost_serialization -lmrmpi

LEX = flex

PROLOG = swipl

include Makefile.targets
