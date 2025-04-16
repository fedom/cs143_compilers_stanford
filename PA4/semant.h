#ifndef SEMANT_H_
#define SEMANT_H_

#include <assert.h>
#include <iostream>
#include <vector>
#include <unordered_map>
#include <cassert>
#include "cool-tree.h"
#include "stringtab.h"
#include "symtab.h"
#include "list.h"
#include "type_environment.h"

#define TRUE 1
#define FALSE 0

class ClassTable;
typedef ClassTable *ClassTableP;

// This is a structure that may be used to contain the semantic
// information such as the inheritance graph.  You may use it or not as
// you like: it is only here to provide a container for the supplied
// methods.

struct ObjInfo
{
  Symbol type;
};


class ClassTable
{
private:
  Classes classes_;
  SymbolTable<Symbol, ObjInfo> sym_table_;
  TypeEnvironment type_env_;

  int semant_errors;
  void install_basic_classes();
  ostream &error_stream;

public:
  ClassTable(Classes);

  void semant();

  int errors() { return semant_errors; }
  ostream &semant_error();
  ostream &semant_error(Class_ c);
  ostream &semant_error(Symbol filename, tree_node *t);
};

#endif