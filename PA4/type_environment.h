#ifndef TYPE_ENVIRONMENT_H_
#define TYPE_ENVIRONMENT_H_

#include <assert.h>
#include <iostream>
#include <vector>
#include <unordered_map>
#include <cassert>
#include "cool-tree.h"
#include "stringtab.h"
#include "symtab.h"
#include "list.h"

typedef Symbol FieldId;
typedef Symbol TypeId;
typedef Symbol ClassId;
typedef Symbol MethodId;
typedef std::vector<TypeId> MethodSignature;

extern Symbol SELF_TYPE;


/*
  SELF_TYPE can appear in different scenarios:
    1. Class field declaration

    // According to test case selftypeparameterposition.test, SELF_TYPE can't be parameter type
    //2. Method's parameter list (like copy())
    
    3. Method return type
    4. New expression
    5. Let expression
    6. Blocks 

    Consider this code example under which circumstances two SELF_TYPE comparison
    might be involved:
    
      class A {
        a : SELF_TYPE;
        foo() : SELF_TYPE {}

        foo2() : Object {
          if (a = foo()) {  // line x
          }
        }
      };

      class B {
        b : SELF_TYPE;
        foo() : Object {
          a : A <- new A
          if (b = a.foo()) {  // line y
          }
        };
      };

    For the case of "line y", a.foo() is the dispatch expression. According to
    the cool manual's type checking, though A::foo() return type is SELF_TYPE,
    a.foo()'s return type should be a concrete type of a. Finally, the type
    of this code line is between SELF_TYPE (since in B it would be B) and A.

    For the case of "line x", foo() is the dispatch expression. It is actually
    self.foo(). According to the cool manual's type checking rule for dispatch,
    self.foo() will return the type of self which is SELF_TYPE. So this code line
    will indeed comparison between SELF_TYPE and SELF_TYPE. And you can notice for
    this scenario they both refer to the same type A. So if we actually meet a
    scenario to compare two types of SELF_TYPE and SELF_TYPE, they must be identical.

  For SymbolTable:
    1. The object's type can be SELF_TYPE(which is wrapped in ObjInfo)

  For TypeEnvironment:
    1. The key of class_map_ would be the Symbol of class name. Can't be SELF_TYPE
    2. inheritance_graph_ doesn't contain SELF_TYPE
    3. cur_class_ won't be SELF_TYPE

  For ClassEntry:
    1. SELF_TYPE can appear in the field_map_'s value. It means the class's field is
      declared type SELF_TYPE. 
    2. SELF_TYPE can appear in the MethodSignature's elements. It means the parameter
      or return type is declared with SELF_TYPE.

  Type compare involving SELF_TYPE:
    1. If two expression 
*/
class TypeEnvironment
{
public:


  class InheritanceGraph
  {
  public:
    Symbol get_parent(Symbol sym) const {return inheritance_tree_.at(sym);}
    void insert(Symbol child, Symbol parent);
    bool le(Symbol lhs, Symbol rhs) const;
    bool lt(Symbol lhs, Symbol rhs) const;

    Symbol join(Symbol lhs, Symbol rhs);

    void assure_acyclic() const;

  private:
    std::unordered_map<Symbol, Symbol> inheritance_tree_;
  };

  // Since method can be inherited and override, it needs a class id to distinguish different versions.
  

  class ClassEntry
  {
  public:
    void insert_field(FieldId field_id, TypeId type_id)
    {
      field_map_[field_id] = type_id;
    }

    void insert_method(MethodId method_id, MethodSignature method_sig)
    {
      method_map_[method_id] = method_sig;
    }

    MethodSignature* get_method_signature(MethodId method_id) {
      auto iter = method_map_.find(method_id);
      return iter == method_map_.end() ? nullptr : &iter->second;
    }

    TypeId get_field_type(FieldId field_id) {
      auto iter = field_map_.find(field_id);

      return iter == field_map_.end() ? nullptr : iter->second;
    }

  private:
    std::unordered_map<FieldId, TypeId> field_map_;
    std::unordered_map<MethodId, MethodSignature> method_map_;
  };

  void insert_class(Symbol child, Symbol parent);
  void insert_class_field(Symbol cls_id, Symbol field_id, Symbol type_id);
  void insert_class_method(Symbol cls_id, Symbol method_id, MethodSignature method_sig);

  MethodSignature *get_class_method(Symbol cls_id, Symbol method_id);
  Symbol get_class_field(Symbol cls_id, Symbol field_id);


  Symbol get_cur_class()
  {
    return cur_class_;
  }

  void set_cur_class(Symbol cur_class)
  {
    cur_class_ = cur_class;
  }

  void check_class_acyclic();

  bool lt(Symbol lhs, Symbol rhs);
  bool le(Symbol lhs, Symbol rhs);

  bool type_exist(Symbol sym) const {
    return sym == SELF_TYPE || class_map_.find(sym) != class_map_.end();
  }

  Symbol join(Symbol lhs, Symbol rhs) {
    if (lhs == SELF_TYPE && rhs == SELF_TYPE) {
      return SELF_TYPE;
    }
    
    lhs = normalize(lhs);
    rhs = normalize(rhs);

    return inheritance_graph_.join(lhs, rhs);
  }

  inline Symbol normalize(Symbol sym) const {
    return sym == SELF_TYPE ? cur_class_ : sym;
  }

private:

  std::unordered_map<ClassId, ClassEntry> class_map_;
  InheritanceGraph inheritance_graph_;
  Symbol cur_class_;
};

#endif// TYPE_ENVIRONMENT_H_