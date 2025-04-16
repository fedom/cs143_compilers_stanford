#include "type_environment.h"
#include <unordered_set>

extern Symbol
    arg,
    arg2,
    Bool,
    concat,
    cool_abort,
    copy,
    Int,
    in_int,
    in_string,
    IO,
    length,
    Main,
    main_meth,
    No_class,
    No_type,
    Object,
    out_int,
    out_string,
    prim_slot,
    self,
    SELF_TYPE,
    Str,
    str_field,
    substr,
    type_name,
    val;

void TypeEnvironment::InheritanceGraph::insert(Symbol child, Symbol parent)
{
  inheritance_tree_[child] = parent;
}

bool TypeEnvironment::InheritanceGraph::le(Symbol lhs, Symbol rhs) const
{
  return lhs == rhs || lt(lhs, rhs);
}

bool TypeEnvironment::InheritanceGraph::lt(Symbol lhs, Symbol rhs) const
{
  if (inheritance_tree_.find(lhs) == inheritance_tree_.end() || inheritance_tree_.find(lhs) == inheritance_tree_.end()) {
    return false;
  }

  Symbol next = inheritance_tree_.at(lhs);

  while (next != No_class)
  {
    if (next == rhs)
    {
      return true;
    }

    next = inheritance_tree_.at(next);
  }

  return false;
}

void TypeEnvironment::InheritanceGraph::assure_acyclic() const
{
  // TODO
}

Symbol TypeEnvironment::InheritanceGraph::join(Symbol lhs, Symbol rhs)
{
  std::unordered_set<Symbol> symbol_set;

  Symbol node = lhs;
  while (node != No_class)
  {
    symbol_set.insert(node);
    node = inheritance_tree_.at(node);
  }

  node = rhs;
  do
  {
    if (symbol_set.find(node) != symbol_set.end())
    {
      return node;
    }
    node = inheritance_tree_.at(node);
  } while (node != No_class);

  return NULL;
}

// Note : type_id can be SELF_TYPE
void TypeEnvironment::insert_class_field(Symbol cls_id, Symbol field_id, Symbol type_id)
{
  assert(class_map_.find(cls_id) != class_map_.end());

  class_map_[cls_id].insert_field(field_id, type_id);
}

// Note : return_type in MethodSignature can be SELF_TYPE
void TypeEnvironment::insert_class_method(Symbol cls_id, Symbol method_id, MethodSignature method_sig)
{
  class_map_[cls_id].insert_method(method_id, std::move(method_sig));
}

void TypeEnvironment::insert_class(Symbol child, Symbol parent)
{
  inheritance_graph_.insert(child, parent);
  class_map_[child] = ClassEntry();
}

MethodSignature *TypeEnvironment::get_class_method(Symbol cls_id, Symbol method_id)
{
  if (cls_id == SELF_TYPE)
  {
    cls_id = cur_class_;
  }

  while (cls_id != No_class)
  {
    if (class_map_.find(cls_id) == class_map_.end())
    {
      break;
    }

    MethodSignature *method_sig = class_map_.at(cls_id).get_method_signature(method_id);
    if (method_sig)
    {
      return method_sig;
    }

    cls_id = inheritance_graph_.get_parent(cls_id);
  }

  return NULL;
}

// Note : the return value can be SELF_TYPE
Symbol TypeEnvironment::get_class_field(Symbol cls_id, Symbol field_id)
{
  if (cls_id == SELF_TYPE)
  {
    cls_id = cur_class_;
  }

  while (cls_id != No_class)
  {
    if (class_map_.find(cls_id) == class_map_.end())
    {
      break;
    }

    Symbol field_type = class_map_.at(cls_id).get_field_type(field_id);
    if (field_type)
    {
      return field_type;
    }

    cls_id = inheritance_graph_.get_parent(cls_id);
  }

  return NULL;
}

void TypeEnvironment::check_class_acyclic()
{
  inheritance_graph_.assure_acyclic();
}

bool TypeEnvironment::le(Symbol lhs, Symbol rhs)
{
  /*

    1. In cool language, we will never meet cases that comparing two SELF_TYPE from different
       classes. So if both lhs and rhs are SELF_TYPE, they are equal.
    2. SELF_TYPE_C <= T is true if C <= T (SELF_TYPE_C means SELF_TYPE appear in class C)
    3. T <= SELF_TYPE_C : This is always false. (SELF_TYPE_C's range is (-inf, C])

    Note: So SELF_TYPE_C can only put on the left of <=. When it is on the right hand side, it
          is always false.
  */

  // Refer to the header file's comment
  if (lhs == SELF_TYPE && rhs == SELF_TYPE) {
    return true;
  }

  // This case is a bit tricky. No concrete type will conform to SELF_TYPE
  // because SELF_TYPE can't be guaranteed being acestor of a concrete type.
  // We can simply create new child class inheriting from it to make this 
  // comparison fail.
  if (rhs == SELF_TYPE) {
    return false;
  }

  lhs = (lhs == SELF_TYPE ? cur_class_ : lhs);

  return inheritance_graph_.le(lhs, rhs);
}