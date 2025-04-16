

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <unordered_set>
#include "semant.h"
#include "utilities.h"
#include "type_environment.h"


extern int semant_debug;
extern char *curr_filename;
extern Program ast_root;

//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
Symbol
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
//
// Initializing the predefined symbols.
//
void initialize_constants(void)
{
    arg = idtable.add_string("arg");
    arg2 = idtable.add_string("arg2");
    Bool = idtable.add_string("Bool");
    concat = idtable.add_string("concat");
    cool_abort = idtable.add_string("abort");
    copy = idtable.add_string("copy");
    Int = idtable.add_string("Int");
    in_int = idtable.add_string("in_int");
    in_string = idtable.add_string("in_string");
    IO = idtable.add_string("IO");
    length = idtable.add_string("length");
    Main = idtable.add_string("Main");
    main_meth = idtable.add_string("main");
    //   _no_class is a symbol that can't be the name of any
    //   user-defined class.
    No_class = idtable.add_string("_no_class");
    No_type = idtable.add_string("_no_type");
    Object = idtable.add_string("Object");
    out_int = idtable.add_string("out_int");
    out_string = idtable.add_string("out_string");
    prim_slot = idtable.add_string("_prim_slot");
    self = idtable.add_string("self");
    SELF_TYPE = idtable.add_string("SELF_TYPE");
    Str = idtable.add_string("String");
    str_field = idtable.add_string("_str_field");
    substr = idtable.add_string("substr");
    type_name = idtable.add_string("type_name");
    val = idtable.add_string("_val");
}

#define ERROR_STREAM() error_log->semant_error(cur_cls->get_filename(), this)

#define CALL_SEMANT(node) node->semant(pass, error_log, cur_cls, sym_table, type_env)

ClassTable::ClassTable(Classes classes) : semant_errors(0), error_stream(cerr)
{

    /* Fill this in */

    classes_ = list_node<Class_>::nil();

    install_basic_classes();
    classes_ = append_Classes(classes_, classes);

    // // Add classes to the type_environment and build the inheritance graph
    // for(int i = classes_->first(); classes_->more(i); i = classes_->next(i)) {
    //      Class_ cls = classes_->nth(i);
    //      type_env_.insert_class(cls->get_name(), cls->get_parent());
    // }

    // type_env_.check_class_acyclic();
}

void ClassTable::install_basic_classes()
{

    // The tree package uses these globals to annotate the classes built below.
    // curr_lineno  = 0;
    Symbol filename = stringtable.add_string("<basic class>");

    // The following demonstrates how to create dummy parse trees to
    // refer to basic Cool classes.  There's no need for method
    // bodies -- these are already built into the runtime system.

    // IMPORTANT: The results of the following expressions are
    // stored in local variables.  You will want to do something
    // with those variables at the end of this method to make this
    // code meaningful.

    //
    // The Object class has no parent class. Its methods are
    //        abort() : Object    aborts the program
    //        type_name() : Str   returns a string representation of class name
    //        copy() : SELF_TYPE  returns a copy of the object
    //
    // There is no need for method bodies in the basic classes---these
    // are already built in to the runtime system.

    Class_ Object_class =
        class_(Object,
               No_class,
               append_Features(
                   append_Features(
                       single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
                       single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
                   single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
               stringtable.add_string("<basic class Object>"));

    //
    // The IO class inherits from Object. Its methods are
    //        out_string(Str) : SELF_TYPE       writes a string to the output
    //        out_int(Int) : SELF_TYPE            "    an int    "  "     "
    //        in_string() : Str                 reads a string from the input
    //        in_int() : Int                      "   an int     "  "     "
    //
    Class_ IO_class =
        class_(IO,
               Object,
               append_Features(
                   append_Features(
                       append_Features(
                           single_Features(method(out_string, single_Formals(formal(arg, Str)),
                                                  SELF_TYPE, no_expr())),
                           single_Features(method(out_int, single_Formals(formal(arg, Int)),
                                                  SELF_TYPE, no_expr()))),
                       single_Features(method(in_string, nil_Formals(), Str, no_expr()))),
                   single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
               stringtable.add_string("<basic class IO>"));

    //
    // The Int class has no methods and only a single attribute, the
    // "val" for the integer.
    //
    Class_ Int_class =
        class_(Int,
               Object,
               single_Features(attr(val, prim_slot, no_expr())),
               stringtable.add_string("<basic class Int>"));

    //
    // Bool also has only the "val" slot.
    //
    Class_ Bool_class =
        class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())), stringtable.add_string("<basic class Bool>"));

    //
    // The class Str has a number of slots and operations:
    //       val                                  the length of the string
    //       str_field                            the string itself
    //       length() : Int                       returns length of the string
    //       concat(arg: Str) : Str               performs string concatenation
    //       substr(arg: Int, arg2: Int): Str     substring selection
    //
    Class_ Str_class =
        class_(Str,
               Object,
               append_Features(
                   append_Features(
                       append_Features(
                           append_Features(
                               single_Features(attr(val, Int, no_expr())),
                               single_Features(attr(str_field, prim_slot, no_expr()))),
                           single_Features(method(length, nil_Formals(), Int, no_expr()))),
                       single_Features(method(concat,
                                              single_Formals(formal(arg, Str)),
                                              Str,
                                              no_expr()))),
                   single_Features(method(substr,
                                          append_Formals(single_Formals(formal(arg, Int)),
                                                         single_Formals(formal(arg2, Int))),
                                          Str,
                                          no_expr()))),
               stringtable.add_string("<basic class Str>"));

    // Ignore the memory management
    classes_ = append_Classes(classes_, single_Classes(Object_class));
    classes_ = append_Classes(classes_, single_Classes(IO_class));
    classes_ = append_Classes(classes_, single_Classes(Int_class));
    classes_ = append_Classes(classes_, single_Classes(Bool_class));
    classes_ = append_Classes(classes_, single_Classes(Str_class));
}

void ClassTable::semant()
{

    for (int i = classes_->first(); classes_->more(i); i = classes_->next(i))
    {
        // Collect all the classes information in first pass
        classes_->nth(i)->semant(1, this, NULL, &sym_table_, &type_env_);
    }

    if (!type_env_.type_exist(Main))
    {
        semant_errors++;
        error_stream << "Class Main is not defined.\n";
    }

    for (int i = classes_->first(); classes_->more(i); i = classes_->next(i))
    {
        classes_->nth(i)->semant(2, this, NULL, &sym_table_, &type_env_);
    }
}

////////////////////////////////////////////////////////////////////
//
// semant_error is an overloaded function for reporting errors
// during semantic analysis.  There are three versions:
//
//    ostream& ClassTable::semant_error()
//
//    ostream& ClassTable::semant_error(Class_ c)
//       print line number and filename for `c'
//
//    ostream& ClassTable::semant_error(Symbol filename, tree_node *t)
//       print a line number and filename
//
///////////////////////////////////////////////////////////////////

ostream &ClassTable::semant_error(Class_ c)
{
    return semant_error(c->get_filename(), c);
}

ostream &ClassTable::semant_error(Symbol filename, tree_node *t)
{
    error_stream << filename << ":" << t->get_line_number() << ": ";
    return semant_error();
}

ostream &ClassTable::semant_error()
{
    semant_errors++;
    return error_stream;
}

/*   This is the entry point to the semantic checker.

     Your checker should do the following two things:

     1) Check that the program is semantically correct
     2) Decorate the abstract syntax tree with type information
        by setting the `type' field in each Expression node.
        (see `tree.h')

     You are free to first do 1), make sure you catch all semantic
     errors. Part 2) can be done in a second stage, when you want
     to build mycoolc.
 */
void program_class::semant()
{
    initialize_constants();

    /* ClassTable constructor may do some semantic analysis */
    ClassTable *classtable = new ClassTable(classes);

    /* some semantic analysis code may go here */
    classtable->semant();

    if (classtable->errors())
    {
        cerr << "Compilation halted due to static semantic errors." << endl;
        exit(1);
    }
}

void class__class::semant(int pass, ClassTable *error_log, Class_ cur_cls, SymbolTable<Symbol, ObjInfo> *sym_table, TypeEnvironment *type_env)
{
    // First round we only collect all the classes' method information
    cur_cls = this;
    type_env->set_cur_class(name);

    if (pass == 1)
    {
        if (type_env->type_exist(name))
        {
            ERROR_STREAM() << " type " << name << " redefinition\n";
            return;
        }

        if (parent == Bool || parent == Str || parent == Int || parent == SELF_TYPE)
        {
            ERROR_STREAM() << " inherit from basic class " << parent << " is not allowed\n";
            type_env->insert_class(name, Object);
        }
        else
        {
            type_env->insert_class(name, parent);
        }

        for (int i = features->first(); features->more(i); i = features->next(i))
        {
            features->nth(i)->semant(pass, error_log, this, sym_table, type_env);
        }
        return;
    }

    // pass == 2
    if (name != Object && !type_env->type_exist(parent))
    {
        ERROR_STREAM() << " " << name << " inherit from undefined class " << parent << "\n";
    }

    sym_table->enterscope();

    // bound self to current class type
    sym_table->addid(self, new ObjInfo{SELF_TYPE});

    for (int i = features->first(); features->more(i); i = features->next(i))
    {
        features->nth(i)->check_override_restrictions(parent, error_log, this, sym_table, type_env);
        features->nth(i)->semant(pass, error_log, this, sym_table, type_env);
    }

    sym_table->exitscope();
}


void method_class::check_override_restrictions(Symbol parent,
    ClassTable *error_log,
    Class_ cur_cls,
    SymbolTable<Symbol, ObjInfo> *sym_table,
    TypeEnvironment *type_env)
{
MethodSignature *sig = type_env->get_class_method(parent, name);
if (!sig)
return;

if (formals->len() != sig->size() - 1) {
ERROR_STREAM() << " override method " << name << " param count doesn't match\n";
return;
}

for (int i = formals->first(); formals->more(i); i = formals->next(i)) {
Symbol parent_param_type = sig->at(i);
Symbol cur_param_type = formals->nth(i)->get_type();
if (cur_param_type != parent_param_type) {
ERROR_STREAM() << " override method " << name << " "
<< i << "th param type " <<  cur_param_type
<< " does not match with original "
<< parent_param_type << "\n"; 
}
}

if (sig->back() != return_type) {
ERROR_STREAM() << " override method " << name << " return type "
<< return_type << " does not match with original "
<< sig->back() << "\n"; 
}
}


void method_class::semant(int pass, ClassTable *error_log,
                          Class_ cur_cls,
                          SymbolTable<Symbol, ObjInfo> *sym_table,
                          TypeEnvironment *type_env)
{
    // pass 1
    if (pass == 1)
    {
        /*
        One of the major themes of modern programming languages is information hiding, which is the
        idea that certain aspects of a data type’s implementation should be abstract and hidden
        from users of the data type. Cool supports information hiding through a simple mechanism:
        all attributes have scope local to the class, and all methods have global scope. Thus, the
        only way to provide access to object state in Cool is through methods.
        */
        std::vector<Symbol> types;
        for (int i = formals->first(); formals->more(i); i = formals->next(i))
        {
            formals->nth(i)->semant(pass, error_log, cur_cls, sym_table, type_env);

            if (formals->nth(i)->get_type() == SELF_TYPE)
            {
                // Error display moved in to formal_class
                // ERROR_STREAM() << " parameter type can't be SELF_TYPE\n";
                types.push_back(Object);
            }
            else
            {
                types.push_back(formals->nth(i)->get_type());
            }
        }
        types.push_back(return_type);
        type_env->insert_class_method(cur_cls->get_name(), name, std::move(types));
        return;
    }

    // pass 2
    // 1. Collect parameters' symbol and type to a new inner scope for method body
    sym_table->enterscope();

    for (int i = formals->first(); formals->more(i); i = formals->next(i))
    {
        Symbol param_name = formals->nth(i)->get_name();
        Symbol param_type = formals->nth(i)->get_type();
        if (sym_table->probe(param_name))
        {
            ERROR_STREAM() << " redefinition for the same param name " << name << "\n";
            continue;
        }
        sym_table->addid(param_name, new ObjInfo{param_type});
    }

    if (!type_env->type_exist(return_type))
    {
        ERROR_STREAM() << " return type " << return_type << " undefined\n";
    }

    // 2. Recursive deep down the AST
    expr->semant(pass, error_log, cur_cls, sym_table, type_env);

    // 3. Method return type check. (type of method body expr should be already set during expr->semant())
    Symbol expr_type = expr->get_type();

    // We check here because we need to distinguish no_expr_class, which we don't set_type() and expr->get_type()
    // should return NULL. In that case, we shouldn't do the type following check.
    if (expr_type)
    {
        // For method definition, since it is not dispatch, we can know where method with be called. So if return
        // type is SELF_TYPE, then the expr's type must be SELF_TYPE instead of a concrete type (or we definitly
        // can create a new child class X and call this method on it which will lead the expr's type unconfrom to
        // SELF_TYPE which would be X).

        // We have merge this case into type_env->le(). Please see the comment there.
        // if (return_type == SELF_TYPE && expr_type != SELF_TYPE)
        // {
        //    ERROR_STREAM() << " type " << expr_type
        //                   << " is incompatible with return type "
        //                   << return_type
        //                   << "\n";
        // }

        if (!type_env->le(expr_type, return_type))
        {
            ERROR_STREAM() << " type" << expr_type
                           << " is incompatible with return type "
                           << return_type
                           << "\n";
        }
    }

    // Exit scope for method body
    sym_table->exitscope();
}

void attr_class::check_override_restrictions(Symbol parent,
                                             ClassTable *error_log,
                                             Class_ cur_cls,
                                             SymbolTable<Symbol, ObjInfo> *sym_table,
                                             TypeEnvironment *type_env)
{
    if (type_env->get_class_field(parent, name))
    {
        ERROR_STREAM() << " attribute " << name << " override not allowed\n";
    }
}

void attr_class::semant(int pass,
                        ClassTable *error_log,
                        Class_ cur_cls,
                        SymbolTable<Symbol, ObjInfo> *sym_table,
                        TypeEnvironment *type_env)
{
    if (pass == 1)
    {
        if (name == self)
        {
            ERROR_STREAM() << " attr name can't be self\n";
        }
        type_env->insert_class_field(type_env->get_cur_class(), name, type_decl);
        return;
    }

    // pass 2

    // Put recursive expression process in pass 2 so that the code will have a initial global view of all
    // the classes.
    if (init)
    {
        CALL_SEMANT(init);
        if (init->get_type())
        {
            // We have merge this case into type_env->le(). Please see the comment there.
            // if (type_decl == SELF_TYPE && init->get_type() != SELF_TYPE)
            // {
            //    ERROR_STREAM() << " init expression type " << init->get_type()
            //                   << " is uncompatible with declared type "
            //                   << type_decl
            //                   << "\n";
            // }

            if (!type_env->le(init->get_type(), type_decl))
            {
                ERROR_STREAM() << " init expression type " << init->get_type()
                               << " is incompatible with declared type "
                               << type_env->normalize(type_decl)
                               << "\n";
            }
        }
    }
}

void formal_class::semant(int pass, ClassTable *error_log,
                          Class_ cur_cls,
                          SymbolTable<Symbol, ObjInfo> *sym_table,
                          TypeEnvironment *type_env)
{
    // Move this code to the caller in method_class
    // sym_table->addid(name, new ObjInfo);
    // type_env->insert_object(name, type_decl);
    if (name == self)
    {
        ERROR_STREAM() << " name can't be self\n";
    }
    if (type_decl == SELF_TYPE)
    {
        ERROR_STREAM() << " formal type can't be SELF_TYPE\n";
    }
}

void branch_class::semant(int pass,
                          ClassTable *error_log,
                          Class_ cur_cls,
                          SymbolTable<Symbol, ObjInfo> *sym_table,
                          TypeEnvironment *type_env)
{
    // Symbol name;
    // Symbol type_decl;
    // Expression expr;

    sym_table->enterscope();

    if (!type_env->type_exist(type_decl))
    {
        ERROR_STREAM() << " " << type_decl << " undefine type\n";
    }
    sym_table->addid(name, new ObjInfo{type_decl});
    CALL_SEMANT(expr);

    sym_table->exitscope();
}

void assign_class::semant(int pass, ClassTable *error_log,
                          Class_ cur_cls,
                          SymbolTable<Symbol, ObjInfo> *sym_table,
                          TypeEnvironment *type_env)
{
    /*
    1. Check symbol availability
    2. Check type compatibility
    3. Set current expression's type
    */
    Symbol decl_type = Object;

    if (name == self)
    {
        ERROR_STREAM() << " can't assign to self\n";
    }

    ObjInfo *p = sym_table->lookup(name);

    if (p)
    {
        decl_type = p->type;
    }
    else
    {
        Symbol field_type = type_env->get_class_field(SELF_TYPE, name);
        if (!field_type)
        {
            ERROR_STREAM() << " undefine the variable " << name << "\n";
        }

        decl_type = field_type ? field_type : Object;
    }

    expr->semant(pass, error_log, cur_cls, sym_table, type_env);

    Symbol expr_type = expr->get_type();

    // no_expr_class will be NULL here
    if (expr_type)
    {
        if (!type_env->le(expr_type, decl_type))
        {
            ERROR_STREAM() << " type " << expr_type << " incompatible with " << decl_type << "\n";
        }

        this->set_type(expr_type);
    }
    else
    {
        this->set_type(decl_type);
    }
}

void static_dispatch_class::semant(int pass,
                                   ClassTable *error_log,
                                   Class_ cur_cls,
                                   SymbolTable<Symbol, ObjInfo> *sym_table,
                                   TypeEnvironment *type_env)
{
    // Set default type for expression
    this->set_type(Object);

    expr->semant(pass, error_log, cur_cls, sym_table, type_env);
    Symbol type_e0 = expr->get_type();

    if (!type_env->le(type_e0, type_name))
    {
        ERROR_STREAM() << " Expression type " << type_e0 << " does not conform to declared static dispatch type "
                       << type_name << "\n";
    }

    MethodSignature *method_sig = type_env->get_class_method(type_name, name);

    if (!method_sig)
    {
        ERROR_STREAM() << " no method " << name << " found in type " << type_name << "\n";
    }

    for (int i = actual->first(); actual->more(i); i = actual->next(i))
    {
        actual->nth(i)->semant(pass, error_log, cur_cls, sym_table, type_env);

        if (!method_sig)
            continue;

        Symbol actual_type = actual->nth(i)->get_type();
        Symbol decl_type = method_sig->at(i);

        if (!type_env->le(actual_type, decl_type))
        {
            ERROR_STREAM() << i << "th argument type " << actual_type << " does not conform to " << decl_type << "\n";
        }
    }

    if (method_sig->back() == SELF_TYPE)
    {
        this->set_type(type_e0);
    }
    else
    {
        this->set_type(method_sig->back());
    }
}

void dispatch_class::semant(int pass,
                            ClassTable *error_log,
                            Class_ cur_cls,
                            SymbolTable<Symbol, ObjInfo> *sym_table,
                            TypeEnvironment *type_env)
{
    this->set_type(Object);

    expr->semant(pass, error_log, cur_cls, sym_table, type_env);
    Symbol type_e0 = expr->get_type();

    MethodSignature *method_sig = type_env->get_class_method(type_e0, name);

    if (!method_sig)
    {
        ERROR_STREAM() << " undefined method name " << name << "\n";
    }

    for (int i = actual->first(); actual->more(i); i = actual->next(i))
    {
        actual->nth(i)->semant(pass, error_log, cur_cls, sym_table, type_env);
        if (!method_sig)
            continue;

        Symbol actual_type = actual->nth(i)->get_type();
        Symbol decl_type = method_sig->at(i);

        if (!type_env->le(actual_type, decl_type))
        {
            ERROR_STREAM() << " param " << i << " type " << actual_type
                           << " incompatible with " << decl_type << "\n";
        }
    }

    if (method_sig)
    {
        if (method_sig->back() == SELF_TYPE)
        {
            this->set_type(type_e0);
        }
        else
        {
            this->set_type(method_sig->back());
        }
    }
    else
    {
        this->set_type(Object);
    }
}

void cond_class::semant(int pass,
                        ClassTable *error_log,
                        Class_ cur_cls,
                        SymbolTable<Symbol, ObjInfo> *sym_table,
                        TypeEnvironment *type_env)

{
    /*
    1. Set expression's default type to Object
    */
    this->set_type(Object);

    pred->semant(pass, error_log, cur_cls, sym_table, type_env);
    then_exp->semant(pass, error_log, cur_cls, sym_table, type_env);
    else_exp->semant(pass, error_log, cur_cls, sym_table, type_env);

    if (pred->get_type() != Bool)
    {
        error_log->semant_error(cur_cls->get_filename(), this);
        return;
    }

    // Note : join() operation accept SELF_TYPE. See cool manual 7.5
    //    join(SELF_TYPE_D, A) = join(D, A)
    Symbol join_symbol = type_env->join(then_exp->get_type(), else_exp->get_type());
    if (!join_symbol)
    {
        error_log->semant_error(cur_cls->get_filename(), this);
        return;
    }

    this->set_type(join_symbol);
}

void loop_class::semant(int pass,
                        ClassTable *error_log,
                        Class_ cur_cls,
                        SymbolTable<Symbol, ObjInfo> *sym_table,
                        TypeEnvironment *type_env)
{
    this->set_type(Object);
    // Expression pred;
    // Expression body;

    CALL_SEMANT(pred);
    if (pred->get_type() != Bool)
    {
        ERROR_STREAM() << " pred's type isn't a valid Bool\n";
    }
    CALL_SEMANT(body);
}

void typcase_class::semant(int pass,
                           ClassTable *error_log,
                           Class_ cur_cls,
                           SymbolTable<Symbol, ObjInfo> *sym_table,
                           TypeEnvironment *type_env)

{
    this->set_type(Object);

    std::unordered_set<Symbol> types;
    Symbol join_sym = NULL;

    // Expression expr;
    // Cases cases;

    expr->semant(pass, error_log, cur_cls, sym_table, type_env);

    for (int i = cases->first(); cases->more(i); i = cases->next(i))
    {
        Symbol decl_type = cases->nth(i)->get_decl_type();
        if (types.find(decl_type) != types.end())
        {
            ERROR_STREAM() << " Duplicate branch " << decl_type << " in case statment\n";
        }
        else
        {
            types.insert(decl_type);
        }

        cases->nth(i)->semant(pass, error_log, cur_cls, sym_table, type_env);

        if (join_sym)
        {
            join_sym = type_env->join(join_sym, cases->nth(i)->get_expr_type());
        }
        else
        {
            join_sym = cases->nth(i)->get_expr_type();
        }
    }

    this->set_type(join_sym);
}

void block_class::semant(int pass,
                         ClassTable *error_log,
                         Class_ cur_cls,
                         SymbolTable<Symbol, ObjInfo> *sym_table,
                         TypeEnvironment *type_env)

{
    /*
    1. Set expression's default type to Object
    */
    this->set_type(Object);

    for (int i = body->first(); body->more(i); i = body->next(i))
    {
        body->nth(i)->semant(pass, error_log, cur_cls, sym_table, type_env);
        this->set_type(body->nth(i)->get_type());
    }
}

void let_class::semant(int pass,
                       ClassTable *error_log,
                       Class_ cur_cls,
                       SymbolTable<Symbol, ObjInfo> *sym_table,
                       TypeEnvironment *type_env)

{
    /*
    1. Set expression's default type to Object
    */
    this->set_type(Object);

    // Symbol identifier;
    // Symbol type_decl;
    // Expression init;
    // Expression body;

    if (!type_env->type_exist(type_decl))
    {
        ERROR_STREAM() << " type " << type_decl << " undefined type\n";
    }

    init->semant(pass, error_log, cur_cls, sym_table, type_env);

    if (init->get_type())
    {
        Symbol init_type = init->get_type();

        if (!type_env->le(init_type, type_decl))
        {
            ERROR_STREAM() << " type of init expression is incompatible with " << type_decl << "\n";
        }
    }

    sym_table->enterscope();
    if (identifier == self)
    {
        ERROR_STREAM() << " variable name can't be self\n";
    }
    else
    {
        sym_table->addid(identifier, new ObjInfo{type_decl});
    }

    body->semant(pass, error_log, cur_cls, sym_table, type_env);
    sym_table->exitscope();

    this->set_type(body->get_type());
}

void plus_class::semant(int pass,
                        ClassTable *error_log,
                        Class_ cur_cls,
                        SymbolTable<Symbol, ObjInfo> *sym_table,
                        TypeEnvironment *type_env)
{
    CALL_SEMANT(e1);
    CALL_SEMANT(e2);

    if (e1->get_type() != Int || e2->get_type() != Int)
    {
        ERROR_STREAM() << " operant not Int\n";
    }

    this->set_type(Int);
}

void sub_class::semant(int pass,
                       ClassTable *error_log,
                       Class_ cur_cls,
                       SymbolTable<Symbol, ObjInfo> *sym_table,
                       TypeEnvironment *type_env)
{
    CALL_SEMANT(e1);
    CALL_SEMANT(e2);

    if (e1->get_type() != Int || e2->get_type() != Int)
    {
        ERROR_STREAM() << " operant not Int\n";
    }

    this->set_type(Int);
}

void mul_class::semant(int pass,
                       ClassTable *error_log,
                       Class_ cur_cls,
                       SymbolTable<Symbol, ObjInfo> *sym_table,
                       TypeEnvironment *type_env)
{
    CALL_SEMANT(e1);
    CALL_SEMANT(e2);

    if (e1->get_type() != Int || e2->get_type() != Int)
    {
        ERROR_STREAM() << " operant not Int\n";
    }

    this->set_type(Int);
}

void divide_class::semant(int pass,
                          ClassTable *error_log,
                          Class_ cur_cls,
                          SymbolTable<Symbol, ObjInfo> *sym_table,
                          TypeEnvironment *type_env)
{
    CALL_SEMANT(e1);
    CALL_SEMANT(e2);

    if (e1->get_type() != Int || e2->get_type() != Int)
    {
        ERROR_STREAM() << " operant not Int\n";
    }

    this->set_type(Int);
}
void neg_class::semant(int pass,
                       ClassTable *error_log,
                       Class_ cur_cls,
                       SymbolTable<Symbol, ObjInfo> *sym_table,
                       TypeEnvironment *type_env)

{
    CALL_SEMANT(e1);
    if (e1->get_type() != Int)
    {
        ERROR_STREAM() << " operant not Int\n";
    }
    this->set_type(Int);
}

void lt_class::semant(int pass,
                      ClassTable *error_log,
                      Class_ cur_cls,
                      SymbolTable<Symbol, ObjInfo> *sym_table,
                      TypeEnvironment *type_env)
{
    CALL_SEMANT(e1);
    CALL_SEMANT(e2);

    if (e1->get_type() != Int || e2->get_type() != Int)
    {
        ERROR_STREAM() << " operant not Int\n";
    }

    this->set_type(Bool);
}

void eq_class::semant(int pass,
                      ClassTable *error_log,
                      Class_ cur_cls,
                      SymbolTable<Symbol, ObjInfo> *sym_table,
                      TypeEnvironment *type_env)
{
    CALL_SEMANT(e1);
    CALL_SEMANT(e2);

    Symbol e1_type = e1->get_type();
    Symbol e2_type = e2->get_type();

    if (e1_type == Int || e1_type == Str || e1_type == Bool || e2_type == Int || e2_type == Str || e2_type == Bool)
    {
        if (e1_type != e2_type)
        {
            ERROR_STREAM() << " operands type must identical for Int/String/Bool\n";
        }
    }

    this->set_type(Bool);
}
void leq_class::semant(int pass,
                       ClassTable *error_log,
                       Class_ cur_cls,
                       SymbolTable<Symbol, ObjInfo> *sym_table,
                       TypeEnvironment *type_env)
{
    CALL_SEMANT(e1);
    CALL_SEMANT(e2);

    if (e1->get_type() != Int || e2->get_type() != Int)
    {
        ERROR_STREAM() << " operant not Int\n";
    }

    this->set_type(Bool);
}

void comp_class::semant(int pass,
                        ClassTable *error_log,
                        Class_ cur_cls,
                        SymbolTable<Symbol, ObjInfo> *sym_table,
                        TypeEnvironment *type_env)
{
    CALL_SEMANT(e1);
    if (e1->get_type() != Bool)
    {
        ERROR_STREAM() << " expression type not Bool\n";
    }

    this->set_type(Bool);
}

void int_const_class::semant(int pass,
                             ClassTable *error_log,
                             Class_ cur_cls,
                             SymbolTable<Symbol, ObjInfo> *sym_table,
                             TypeEnvironment *type_env)

{
    /*
    1. Set expression's type
    */
    this->set_type(Int);
}

void bool_const_class::semant(int pass,
                              ClassTable *error_log,
                              Class_ cur_cls,
                              SymbolTable<Symbol, ObjInfo> *sym_table,
                              TypeEnvironment *type_env)

{
    /*
    1. Set expression's type
    */

    this->set_type(Bool);
}
void string_const_class::semant(int pass, ClassTable *error_log,
                                Class_ cur_cls,
                                SymbolTable<Symbol, ObjInfo> *sym_table,
                                TypeEnvironment *type_env)

{
    /*
    1. Set expression's type
    */

    this->set_type(Str);
}

void new__class::semant(int pass, ClassTable *error_log,
                        Class_ cur_cls,
                        SymbolTable<Symbol, ObjInfo> *sym_table,
                        TypeEnvironment *type_env)

{
    // Note : if type_name is SELF_TYPE, the ret_type should also be SELF_TYPE.
    // It will be determined at runtime for the concrete type.
    Symbol ret_type = type_name;

    if (!type_env->type_exist(ret_type))
    {
        ERROR_STREAM() << " undefined type " << ret_type << "\n";
        this->set_type(Object);
        return;
    }

    this->set_type(ret_type);
}

void isvoid_class::semant(int pass,
                          ClassTable *error_log,
                          Class_ cur_cls,
                          SymbolTable<Symbol, ObjInfo> *sym_table,
                          TypeEnvironment *type_env)
{
    CALL_SEMANT(e1);
    this->set_type(Bool);
}

void no_expr_class::semant(int pass,
                           ClassTable *error_log,
                           Class_ cur_cls,
                           SymbolTable<Symbol, ObjInfo> *sym_table,
                           TypeEnvironment *type_env)
{
    // We can't set type to Object, it will lead method type checking failure which require the expr_ret_type <= type_decl
    // this->set_type(Object);
}

void object_class::semant(int pass,
                          ClassTable *error_log,
                          Class_ cur_cls,
                          SymbolTable<Symbol, ObjInfo> *sym_table,
                          TypeEnvironment *type_env)
{
    this->set_type(Object);

    // Note : ret_type can be SELF_TYPE
    ObjInfo *p = sym_table->lookup(name);
    if (p)
    {
        this->set_type(p->type);
        return;
    }

    Symbol ret_type = type_env->get_class_field(type_env->get_cur_class(), name);

    if (!ret_type)
    {
        ERROR_STREAM() << " undefined variable " << name << "\n";
    }

    this->set_type(ret_type);
}
