#include <assert.h>
#include <stdio.h>
#include <sstream>
#include <vector>
#include <stack>
#include <unordered_map>
#include "emit.h"
#include "cool-tree.h"
#include "symtab.h"

enum Basicness
{
   Basic,
   NotBasic
};
#define TRUE 1
#define FALSE 0

class CgenClassTable;
typedef CgenClassTable *CgenClassTableP;

class CgenNode;
typedef CgenNode *CgenNodeP;

class CgenClassTable : public SymbolTable<Symbol, CgenNode>
{
private:
   List<CgenNode> *nds;
   ostream &str;
   std::vector<CgenNode *> ordered_classes_;

   int stringclasstag;
   int intclasstag;
   int boolclasstag;

   // The following methods emit code for
   // constants and global declarations.

   void code_global_data();
   void code_global_text();
   void code_bools(int);
   void code_select_gc();
   void code_constants();

   // The following creates an inheritance graph from
   // a list of classes.  The graph is implemented as
   // a tree of `CgenNode', and class names are placed
   // in the base class symbol table.

   void install_basic_classes();
   void install_class(CgenNodeP nd);
   void install_classes(Classes cs);
   void build_inheritance_tree();
   void set_relations(CgenNodeP nd);
   void set_class_tags();

   // 
   void code_class_nametab();
   void code_class_objtab();

   // emit code for class protobj
   void code_class_protobjs();
   void code_class_methods();

   void code_class_initializers();

   // Caller guarantee the order of CgenNode
   void emit_nametab_entry(CgenNode *p);
   void emit_objtab_entry(CgenNode *p);

public:
   CgenClassTable(Classes, ostream &str);
   void code();
   CgenNodeP root();
};

class CgenNode : public class__class
{
private:
   CgenNodeP parentnd;       // Parent of class
   List<CgenNode> *children; // Children of class
   Basicness basic_status;   // `Basic' if class is basic
                             // `NotBasic' otherwise

   struct AttrInfo
   {
      Symbol name;
      Symbol type;
      attr_class *ptr;
   };

   struct MethodInfo
   {
      Symbol name;
      Symbol owner_name;
      method_class *method_vec_;
   };

   std::unordered_map<Symbol, size_t> method_map_;
   std::vector<MethodInfo> method_vec_;
   std::unordered_map<Symbol, size_t> attr_map_;
   std::vector<AttrInfo> attr_vec_;
   int class_tag_;
   int max_child_tag_;

   // Caller guarantees this routine is called in a right order of CgenNode.
   // So the parent's features are already prepared.
   void copy_features(
       std::unordered_map<Symbol, size_t> &attr_map,
       std::vector<AttrInfo> &attr_vec,
       std::unordered_map<Symbol, size_t> &method_map,
       std::vector<MethodInfo> &method_vec);

   void collect_self_features();

public:
   CgenNode(Class_ c,
            Basicness bstatus,
            CgenClassTableP class_table);

   void add_child(CgenNodeP child);
   List<CgenNode> *get_children() { return children; }
   void set_parentnd(CgenNodeP p);
   CgenNodeP get_parentnd() { return parentnd; }
   int basic() { return (basic_status == Basic); }

   void set_tag_recursive(int &tag, std::vector<CgenNode *> &ordered_classes);
   int get_tag() { return class_tag_; }
   int get_max_child_tag() {return max_child_tag_;}

   // tag + size + disptab + n * attr
   // This is only valid after the collect_features() has been called.
   int get_protobj_size() {return 3 + attr_vec_.size();}

   // return value is the count of word. Not bytes.
   int get_attr_offset(Symbol sym) {return 3 + attr_map_.at(sym);}
   size_t get_method_index(Symbol method_name) const {return method_map_.at(method_name);}

   // Caller guarantees this routine is called in a right order of CgenNode.
   // So the parent's features are already prepared.
   void collect_features();

   void code_nametab_entry(ostream &s);
   void code_objtab_entry(ostream &s);

   void code_class_protobj(ostream &str);
   void code_class_disptab(ostream &str);
   void code_methods(ostream &os, Environment *env);
   void code_initializer(ostream &os, Environment *env);
};

class BoolConst
{
private:
   int val;

public:
   BoolConst(int);
   void code_def(ostream &, int boolclasstag);
   void code_ref(ostream &) const;
};

struct VarInfo {
   Symbol type;
   bool is_param;
   size_t index;
};

class Environment {
public:
   Environment(SymbolTable<Symbol, CgenNode> *class_tab) : class_tab_(class_tab), var_index_(0) {
   }

   ~Environment() {
   }

   void enterscope() {
      sym_tab_.enterscope();
      var_count_stack_.push(0);
   }

   void exitscope() {
      sym_tab_.exitscope();
      var_index_ -= var_count_stack_.top();
      var_count_stack_.pop();
   }

   void add_param(Symbol sym, Symbol type, size_t i) {
      sym_tab_.addid(sym, new VarInfo{type, true, i});
   }

   void add_var(Symbol sym, Symbol type) {
      sym_tab_.addid(sym, new VarInfo{type, false, var_index_});

      var_count_stack_.top()++;
      ++var_index_;
   }

   size_t get_scope_var_count() {
      return var_count_stack_.top();
   }

   VarInfo *lookup(Symbol sym) {
      return sym_tab_.lookup(sym);
   }

   VarInfo *probe(Symbol sym) {
      return sym_tab_.probe(sym);
   }

   CgenNode *get_class_cgen_node(Symbol sym) {
      return class_tab_->lookup(sym);
   }

   int get_cur_class_attr_offset(Symbol sym) {
      return cur_class_->get_attr_offset(sym);
   }

   int get_method_index(Symbol cls, Symbol method) {
      CgenNode *node = class_tab_->lookup(cls);
      assert(node);

      return node->get_method_index(method);
   }

   void set_current_class(CgenNode *p) { cur_class_ = p;} 
   CgenNode *get_current_class() {return cur_class_;}

private:
   // var_index_ is 0-based
   size_t var_index_;
   std::stack<size_t> var_count_stack_;

   SymbolTable<Symbol, VarInfo>  sym_tab_;
   SymbolTable<Symbol, CgenNode> *class_tab_;
   CgenNode *cur_class_;
};
