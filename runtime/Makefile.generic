# CMakefile: generic automatic Makefile for C and C++
#
# See https://github.com/lukaszcz/cmakefile for the latest version.
#
# Copyright (C) 2008-2022 by Lukasz Czajka.
#
# Distributed under the MIT license. See the bottom of this file.
#

MAKEFILE := $(lastword $(MAKEFILE_LIST))

WORD := [[:graph:]]*[[:space:]][[:space:]]*
CAT_PROJECT := (if [ -f PROJECT ]; then cat PROJECT | sed "s/^\#.*//"; else echo ""; fi)

mstrip = $(patsubst [[:space:]]*%[[:space:]]*,%,$(1))
lsdirs = $(foreach dir,$(2),$(foreach file,$(wildcard $(dir)/*.$(1)),$(file)))

getopt = $(call mstrip,$(shell $(CAT_PROJECT) | sed -n "s/^[[:space:]]*$(1)[[:space:]]*=[[:space:]]*\(.*\)/\1/p"))
getpopt = $(call mstrip,$(foreach cfg,$(1),$(shell $(CAT_PROJECT) | sed -n "s/^[[:space:]]*\($(WORD)\)*$(cfg)[[:space:]][[:space:]]*\($(WORD)\)*$(2)[[:space:]]*=[[:space:]]*\(.*\)/\3/p")))

# build configuration
CONFIG   := $(call getopt,CONFIG)
override CONFIG := $(sort $(strip $(subst +, ,$(CONFIG)) $(CONFIG)))
ifeq ($(CONFIG),)
getcopt = $(call getopt,$(1))
getropt = $(call getopt,$(1))
else
getcopt = $(call mstrip,$(call getopt,$(1)) $(call getpopt,$(CONFIG),$(1)))
getropt = $(if $(call getpopt,$(CONFIG),$(1)),$(call getpopt,$(CONFIG),$(1)),$(call getopt,$(1)))
endif
# getcopt gets a concatenable option; getropt a replaceable option

CC          := $(call getropt,CC)
ifeq ($(CC),)
CC := gcc
endif
CXX	    := $(call getropt,CXX)
ifeq ($(CXX),)
CXX := g++
endif
YACC        := $(call getropt,YACC)
ifeq ($(YACC),)
YACC := bison
endif
LEX         := $(call getropt,LEX)
ifeq ($(LEX),)
LEX := flex
endif
CFLAGS      := $(call getcopt,CFLAGS) $(CFLAGS)
CXXFLAGS    := $(call getcopt,CXXFLAGS) $(CXXFLAGS)
CDEPFLAGS   := $(call getcopt,CDEPFLAGS) $(CDEPFLAGS)
ifeq ($(strip $(CDEPFLAGS)),)
CDEPFLAGS   := $(CFLAGS)
endif
CXXDEPFLAGS := $(call getcopt,CXXDEPFLAGS) $(CXXDEPFLAGS)
ifeq ($(strip $(CXXDEPFLAGS)),)
CXXDEPFLAGS   := $(CXXFLAGS)
endif
LDFLAGS     := $(call getcopt,LDFLAGS) $(LDFLAGS)
CCLDFLAGS   := $(call getcopt,CCLDFLAGS) $(LDFLAGS) $(CCLDFLAGS)
CXXLDFLAGS  := $(call getcopt,CXXLDFLAGS) $(LDFLAGS) $(CXXLDFLAGS)
YFLAGS      := $(call getcopt,YFLAGS) $(YFLAGS)
ifeq ($(strip $(YFLAGS)),)
YFLAGS	    := -d
endif
LEXFLAGS    := $(call getcopt,LEXFLAGS) $(LEXFLAGS)
LIBFLAGS    := $(call getcopt,LIBFLAGS) $(LIBFLAGS)
ifeq ($(strip $(LIBFLAGS)),)
LIBFLAGS := -static -o
endif

# source directory
SRCDIR    := $(call getropt,SRCDIR)
ifeq ($(SRCDIR),)
SRCDIR     := $(shell if [ -d src ]; then echo "src/"; else echo ""; fi)
else
override SRCDIR     := $(SRCDIR)/
endif
ifeq ($(SRCDIR),./)
BUILDDIR :=
endif
ifeq ($(SRCDIR),.//)
BUILDDIR :=
endif
ifneq ($(SRCDIR),)
INCLUDES := $(call mstrip,-I$(SRCDIR) $(INCLUDES))
endif

# build directory
BUILDDIR    := $(call getropt,BUILDDIR)
ifeq ($(BUILDDIR),)
BUILDDIR     := _build/
else
override BUILDDIR     := $(BUILDDIR)/
endif
ifeq ($(BUILDDIR),./)
BUILDDIR :=
endif
ifeq ($(BUILDDIR),.//)
BUILDDIR :=
endif
ifneq ($(BUILDDIR),)
INCLUDES := $(call mstrip,-I$(BUILDDIR)$(SRCDIR) $(INCLUDES))
ifeq ($(SRCDIR),)
INCLUDES := $(call mstrip,-I. $(INCLUDES))
endif
endif

# update flags
ifneq ($(INCLUDES),)
CFLAGS := $(INCLUDES) $(CFLAGS)
CXXFLAGS := $(INCLUDES) $(CXXFLAGS)
CDEPFLAGS := $(INCLUDES) $(CDEPFLAGS)
CXXDEPFLAGS := $(INCLUDES) $(CXXDEPFLAGS)
endif

# library to create
LIB	    := $(call getropt,LIB)
ifneq ($(LIB),)
LIB := $(BUILDDIR)lib$(LIB).a
endif
# programs to create
PROGRAMS     := $(patsubst %,$(BUILDDIR)$(SRCDIR)%,$(call getcopt,PROGRAMS))
# subdirectories of the source directory
SUBDIRS     := $(call getcopt,SUBDIRS)
override SUBDIRS := $(patsubst %,$(SRCDIR)%,$(SUBDIRS))
# recursive subdirectories of the source directory
RSUBDIRS     := $(call getcopt,RSUBDIRS)
override RSUBDIRS := $(patsubst %,$(SRCDIR)%,$(RSUBDIRS))
override SUBDIRS := $(strip $(SUBDIRS) $(foreach dir,$(RSUBDIRS),$(shell find $(dir) -type d)))
# files to ignore
IGNORE	    := $(patsubst %,$(SRCDIR)%,$(call getcopt,IGNORE))
# source files
YSOURCES    := $(filter-out $(IGNORE),$(wildcard $(SRCDIR)*.y) $(call lsdirs,y,$(SUBDIRS)))
LEXSOURCES  := $(filter-out $(IGNORE),$(wildcard $(SRCDIR)*.lex) $(call lsdirs,lex,$(SUBDIRS)))
CYSOURCES   := $(patsubst %.y,$(BUILDDIR)%.c,$(YSOURCES))
HYSOURCES   := $(patsubst %.y,$(BUILDDIR)%.h,$(YSOURCES))
CLEXSOURCES := $(patsubst %.lex,$(BUILDDIR)%.c,$(LEXSOURCES))
CSOURCES    := $(filter-out $(IGNORE) $(CYSOURCES) $(CLEXSOURCES),$(strip $(sort $(wildcard $(SRCDIR)*.c) $(call lsdirs,c,$(SUBDIRS)))))
ALLCSOURCES := $(filter-out $(IGNORE),$(strip $(CSOURCES) $(CYSOURCES) $(CLEXSOURCES)))
CPPSOURCES  := $(filter-out $(IGNORE),$(strip $(wildcard $(SRCDIR)*.cpp) $(call lsdirs,cpp,$(SUBDIRS))))
CXXSOURCES  := $(filter-out $(IGNORE),$(strip $(wildcard $(SRCDIR)*.cxx) $(call lsdirs,cxx,$(SUBDIRS))))
CCSOURCES  := $(filter-out $(IGNORE),$(strip $(wildcard $(SRCDIR)*.cc) $(call lsdirs,cc,$(SUBDIRS))))
ALLCPPSOURCES := $(strip $(CPPSOURCES) $(CXXSOURCES) $(CCSOURCES))

# all object files (sort to remove duplicates, which may exist from
# previous builds in a different directory)
COBJECTS    := $(sort $(patsubst %.c,$(BUILDDIR)%.o,$(CSOURCES)) $(patsubst %.c,%.o,$(CYSOURCES) $(CLEXSOURCES)))
CPPOBJECTS  := $(patsubst %.cpp,$(BUILDDIR)%.o,$(CPPSOURCES))
CXXOBJECTS  := $(patsubst %.cxx,$(BUILDDIR)%.o,$(CXXSOURCES))
CCOBJECTS   := $(patsubst %.cc,$(BUILDDIR)%.o,$(CCSOURCES))
ALLCPPOBJECTS := $(strip $(CPPOBJECTS) $(CXXOBJECTS) $(CCOBJECTS))
ALLOBJECTS  := $(strip $(COBJECTS) $(ALLCPPOBJECTS))

# object files which contain the "main" function
ifneq ($(strip $(CSOURCES)),)
   CMAINOBJECTS := $(patsubst %.c,$(BUILDDIR)%.o,$(shell egrep -l '\bint[[:space:]]+main\b' $(CSOURCES)))
else
   CMAINOBJECTS :=
endif
ifneq ($(strip $(CPPSOURCES)),)
   CPPMAINOBJECTS := $(patsubst %.cpp,$(BUILDDIR)%.o,$(shell egrep -l '\bint[[:space:]]+main\b' $(CPPSOURCES)))
else
   CPPMAINOBJECTS :=
endif
ifneq ($(strip $(CXXSOURCES)),)
   CXXMAINOBJECTS := $(patsubst %.cxx,$(BUILDDIR)%.o,$(shell egrep -l 'int[[:space:]]+main\b' $(CXXSOURCES)))
else
   CXXMAINOBJECTS :=
endif
ifneq ($(strip $(CCSOURCES)),)
   CCMAINOBJECTS := $(patsubst %.cxx,$(BUILDDIR)%.o,$(shell egrep -l 'int[[:space:]]+main\b' $(CCSOURCES)))
else
   CCMAINOBJECTS :=
endif
ifneq ($(PROGRAMS),)
MAINOBJECTS  := $(patsubst %,%.o,$(PROGRAMS))
CPROGRAMS    := $(filter $(PROGRAMS),$(patsubst %.o,%,$(COBJECTS)))
CPPPROGRAMS  := $(filter $(PROGRAMS),$(patsubst %.o,%,$(ALLCPPOBJECTS)))
else ifneq ($(LIB),)
MAINOBJECTS  :=
CPROGRAMS    :=
CPPPROGRAMS  :=
else
MAINOBJECTS  := $(CMAINOBJECTS) $(CPPMAINOBJECTS) $(CXXMAINOBJECTS) $(CCMAINOBJECTS)
CPROGRAMS    := $(patsubst %.o,%,$(CMAINOBJECTS))
CPPPROGRAMS  := $(patsubst %.o,%,$(CPPMAINOBJECTS)) $(patsubst %.o,%,$(CXXMAINOBJECTS)) $(patsubst %.o,%,$(CCMAINOBJECTS))
PROGRAMS     := $(patsubst %.o,%,$(MAINOBJECTS))
endif
# dependencies for each source file
CDEPENDS     := $(patsubst %.c,$(BUILDDIR)%.d,$(CSOURCES))
CYLDEPENDS   := $(patsubst %.c,%.d,$(CYSOURCES)) $(patsubst %.c,%.d,$(CLEXSOURCES))
CPPDEPENDS   := $(patsubst %.cpp,$(BUILDDIR)%.d,$(CPPSOURCES))
CXXDEPENDS   := $(patsubst %.cxx,$(BUILDDIR)%.d,$(CXXSOURCES))
CCDEPENDS    := $(patsubst %.cc,$(BUILDDIR)%.d,$(CCSOURCES))
DEPENDS := $(sort $(CDEPENDS) $(CYLDEPENDS) $(CPPDEPENDS) $(CXXDEPENDS) $(CCDEPENDS))
# object files which don't include the "main" function
OBJECTS	    := $(filter-out $(MAINOBJECTS),$(ALLOBJECTS))

# linkers
CCLD	    := $(call getropt,CCLD)
ifeq ($(CCLD),)
ifeq ($(ALLCPPSOURCES),)
CCLD := $(CC)
else
CCLD := $(CXX)
endif
endif

CXXLD	    := $(call getropt,CXXLD)
ifeq ($(CXXLD),)
CXXLD := $(CXX)
endif

LIBTOOL	    := $(call getropt,LIBTOOL)
ifeq ($(LIBTOOL),)
LIBTOOL := libtool
endif

ifneq ($(BUILDDIR),)
$(shell mkdir -p $(BUILDDIR))
$(shell mkdir -p $(BUILDDIR)$(SRCDIR))
$(foreach dir,$(patsubst %,$(BUILDDIR)%,$(SUBDIRS)),$(shell mkdir -p $(dir)))
endif

.PHONY: all depend clean

all: $(DEPENDS) $(OBJECTS) $(PROGRAMS) $(LIB)

depend: $(DEPENDS)

$(HYSOURCES) : $(BUILDDIR)%.h : %.y
	$(YACC) $(YFLAGS) -o $(patsubst %.h,%.c,$@) $<

$(CYSOURCES) : $(BUILDDIR)%.c : %.y
	$(YACC) $(YFLAGS) -o $@ $<

$(CLEXSOURCES) : $(BUILDDIR)%.c : %.lex
	$(LEX) $(LEXFLAGS) -o $@ $<

$(CDEPENDS) : $(BUILDDIR)%.d : %.c
	$(CC) $(CDEPFLAGS) -MM -MT $(patsubst %.c,$(BUILDDIR)%.o,$<) $< > $@
	printf "\t$(CC) -c $(CFLAGS) -o $(patsubst %.c,$(BUILDDIR)%.o,$<) $<\n" >> $@

$(CYLDEPENDS) : $(BUILDDIR)%.d : $(BUILDDIR)%.c
	$(CC) $(CDEPFLAGS) -MM -MT $(patsubst %.c,%.o,$<) $< > $@
	printf "\t$(CC) -c $(CFLAGS) -o $(patsubst %.c,%.o,$<) $<\n" >> $@

$(CPPDEPENDS) : $(BUILDDIR)%.d : %.cpp
	$(CXX) $(CXXDEPFLAGS) -MM -MT $(patsubst %.cpp,$(BUILDDIR)%.o,$<) $< > $@
	printf "\t$(CXX) -c $(CXXFLAGS) -o $(patsubst %.cpp,$(BUILDDIR)%.o,$<) $<\n" >> $@

$(CXXDEPENDS) : $(BUILDDIR)%.d : %.cxx
	$(CXX) $(CXXDEPFLAGS) -MM -MT $(patsubst %.cxx,$(BUILDDIR)%.o,$<) $< > $@
	printf "\t$(CXX) -c $(CXXFLAGS) -o $(patsubst %.cxx,$(BUILDDIR)%.o,$<) $<\n" >> $@

$(CCDEPENDS) : $(BUILDDIR)%.d : %.cc
	$(CXX) $(CXXDEPFLAGS) -MM -MT $(patsubst %.cc,$(BUILDDIR)%.o,$<) $< > $@
	printf "\t$(CXX) -c $(CXXFLAGS) -o $(patsubst %.cc,$(BUILDDIR)%.o,$<) $<\n" >> $@

$(CPROGRAMS) : % : $(ALLOBJECTS)
	$(CCLD) -o $@ $@.o $(OBJECTS) $(CCLDFLAGS)

$(CPPPROGRAMS) : % : $(ALLOBJECTS)
	$(CXXLD) -o $@  $@.o $(OBJECTS) $(CXXLDFLAGS)

$(LIB) : % : $(OBJECTS)
	$(LIBTOOL) $(LIBFLAGS) $@ $(OBJECTS)

-include $(DEPENDS)

clean:
	-rm -f $(PROGRAMS) $(ALLOBJECTS) $(DEPENDS) $(LIB) $(CYSOURCES) $(HYSOURCES) $(CLEXSOURCES)
ifneq ($(wildcard $(BUILDDIR).prjconfig),)
	-rm $(BUILDDIR).prjconfig
endif
ifneq ($(BUILDDIR),)
	-rm -rf $(BUILDDIR)
endif

ifneq ($(wildcard PROJECT),)
$(MAKEFILE): $(DEPENDS) $(BUILDDIR).prjconfig

$(BUILDDIR).prjconfig: PROJECT
	-rm -f $(PROGRAMS) $(ALLOBJECTS) $(DEPENDS) $(LIB) $(CYSOURCES) $(HYSOURCES) $(CLEXSOURCES)
	touch $(BUILDDIR).prjconfig
else
$(MAKEFILE): $(DEPENDS)
endif

ifneq ($(wildcard Makefile-include),)
include Makefile-include
endif

# Copyright (c) 2008-2022 by Lukasz Czajka.

# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:

# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.

# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.
