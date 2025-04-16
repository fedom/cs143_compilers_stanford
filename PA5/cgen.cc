
//**************************************************************
//
// Code generator SKELETON
//
// Read the comments carefully. Make sure to
//    initialize the base class tags in
//       `CgenClassTable::CgenClassTable'
//
//    Add the label for the dispatch tables to
//       `IntEntry::code_def'
//       `StringEntry::code_def'
//       `BoolConst::code_def'
//
//    Add code to emit everyting else that is needed
//       in `CgenClassTable::code'
//
//
// The files as provided will produce code to begin the code
// segments, declare globals, and emit constants.  You must
// fill in the rest.
//
//**************************************************************

#include "cgen.h"
#include "cgen_gc.h"
#include <vector>
#include <unordered_map>
#include <queue>
#include <functional>

extern void emit_string_constant(ostream &str, char *s);
extern int cgen_debug;
int label_index = 0;

const int kClassTagOffset = 0;
const int kClassSizeOffset = 1;
const int kDispTabOffset = 2;
const int kFirstValueOffset = 3;
const int kStrLenOffset = 3;
const int kStrCharOffset = 4;
//
// Three symbols from the semantic analyzer (semant.cc) are used.
// If e : No_type, then no code is generated for e.
// Special code is generated for new SELF_TYPE.
// The name "self" also generates code different from other references.
//
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
static void initialize_constants(void)
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

static char *gc_init_names[] =
    {"_NoGC_Init", "_GenGC_Init", "_ScnGC_Init"};
static char *gc_collect_names[] =
    {"_NoGC_Collect", "_GenGC_Collect", "_ScnGC_Collect"};

//  BoolConst is a class that implements code generation for operations
//  on the two booleans, which are given global names here.
BoolConst falsebool(FALSE);
BoolConst truebool(TRUE);

//*********************************************************
//
// Define method for code generation
//
// This is the method called by the compiler driver
// `cgtest.cc'. cgen takes an `ostream' to which the assembly will be
// emmitted, and it passes this and the class list of the
// code generator tree to the constructor for `CgenClassTable'.
// That constructor performs all of the work of the code
// generator.
//
//*********************************************************

void program_class::cgen(ostream &os)
{
  // spim wants comments to start with '#'
  os << "# start of generated code\n";

  initialize_constants();
  CgenClassTable *codegen_classtable = new CgenClassTable(classes, os);

  os << "\n# end of generated code\n";
}

//////////////////////////////////////////////////////////////////////////////
//
//  emit_* procedures
//
//  emit_X  writes code for operation "X" to the output stream.
//  There is an emit_X for each opcode X, as well as emit_ functions
//  for generating names according to the naming conventions (see emit.h)
//  and calls to support functions defined in the trap handler.
//
//  Register names and addresses are passed as strings.  See `emit.h'
//  for symbolic names you can use to refer to the strings.
//
//////////////////////////////////////////////////////////////////////////////

static void emit_load(const char *dest_reg, int offset, const char *source_reg, ostream &s)
{
  s << LW << dest_reg << " " << offset * WORD_SIZE << "(" << source_reg << ")"
    << endl;
}

static void emit_load_byte(const char *dest_reg, int offset, const char *source_reg, ostream &s)
{
  s << LB << dest_reg << " " << offset << "(" << source_reg << ")"
    << endl;
}


static void emit_store(const char *source_reg, int offset, const char *dest_reg, ostream &s)
{
  s << SW << source_reg << " " << offset * WORD_SIZE << "(" << dest_reg << ")"
    << endl;
}

static void emit_load_imm(const char *dest_reg, int val, ostream &s)
{
  s << LI << dest_reg << " " << val << endl;
}

static void emit_load_address(const char *dest_reg, const char *address, ostream &s)
{
  s << LA << dest_reg << " " << address << endl;
}

static void emit_partial_load_address(const char *dest_reg, ostream &s)
{
  s << LA << dest_reg << " ";
}

static void emit_load_bool(const char *dest, const BoolConst &b, ostream &s)
{
  emit_partial_load_address(dest, s);
  b.code_ref(s);
  s << endl;
}

static void emit_load_string(const char *dest, StringEntry *str, ostream &s)
{
  emit_partial_load_address(dest, s);
  str->code_ref(s);
  s << endl;
}

static void emit_load_int(const char *dest, IntEntry *i, ostream &s)
{
  emit_partial_load_address(dest, s);
  i->code_ref(s);
  s << endl;
}

static void emit_move(const char *dest_reg, const char *source_reg, ostream &s)
{
  s << MOVE << dest_reg << " " << source_reg << endl;
}

static void emit_neg(const char *dest, const char *src1, ostream &s)
{
  s << NEG << dest << " " << src1 << endl;
}

static void emit_add(const char *dest, const char *src1, const char *src2, ostream &s)
{
  s << ADD << dest << " " << src1 << " " << src2 << endl;
}

static void emit_addu(const char *dest, const char *src1, const char *src2, ostream &s)
{
  s << ADDU << dest << " " << src1 << " " << src2 << endl;
}

static void emit_addiu(const char *dest, const char *src1, int imm, ostream &s)
{
  s << ADDIU << dest << " " << src1 << " " << imm << endl;
}

static void emit_div(const char *dest, const char *src1, const char *src2, ostream &s)
{
  s << DIV << dest << " " << src1 << " " << src2 << endl;
}

static void emit_mul(const char *dest, const char *src1, const char *src2, ostream &s)
{
  s << MUL << dest << " " << src1 << " " << src2 << endl;
}

static void emit_sub(const char *dest,const char *src1, const char *src2, ostream &s)
{
  s << SUB << dest << " " << src1 << " " << src2 << endl;
}

static void emit_sll(const char *dest, const char *src1, int num, ostream &s)
{
  s << SLL << dest << " " << src1 << " " << num << endl;
}

static void emit_jalr(const char *dest, ostream &s)
{
  s << JALR << "\t" << dest << endl;
}

static void emit_jal(const char *address, ostream &s)
{
  s << JAL << address << endl;
}

static void emit_return(ostream &s)
{
  s << RET << endl;
}

static void emit_gc_assign(ostream &s)
{
  s << JAL << "_GenGC_Assign" << endl;
}

static void emit_disptable_ref(Symbol sym, ostream &s)
{
  s << sym << DISPTAB_SUFFIX;
}

static void emit_init_ref(Symbol sym, ostream &s)
{
  s << sym << CLASSINIT_SUFFIX;
}

static void emit_label_ref(int l, ostream &s)
{
  s << "label" << l;
}

static void emit_protobj_ref(Symbol sym, ostream &s)
{
  s << sym << PROTOBJ_SUFFIX;
}

static void emit_method_ref(Symbol classname, Symbol methodname, ostream &s)
{
  s << classname << METHOD_SEP << methodname;
}

static void emit_label_def(int l, ostream &s)
{
  emit_label_ref(l, s);
  s << ":" << endl;
}

static void emit_beqz(const char *source, int label, ostream &s)
{
  s << BEQZ << source << " ";
  emit_label_ref(label, s);
  s << endl;
}

static void emit_beq(const char *src1, const char *src2, int label, ostream &s)
{
  s << BEQ << src1 << " " << src2 << " ";
  emit_label_ref(label, s);
  s << endl;
}

static void emit_bne(const char *src1, const char *src2, int label, ostream &s)
{
  s << BNE << src1 << " " << src2 << " ";
  emit_label_ref(label, s);
  s << endl;
}

static void emit_bleq(const char *src1, const char *src2, int label, ostream &s)
{
  s << BLEQ << src1 << " " << src2 << " ";
  emit_label_ref(label, s);
  s << endl;
}

static void emit_blt(const char *src1, const char *src2, int label, ostream &s)
{
  s << BLT << src1 << " " << src2 << " ";
  emit_label_ref(label, s);
  s << endl;
}

static void emit_blti(const char *src1, int imm, int label, ostream &s)
{
  s << BLT << src1 << " " << imm << " ";
  emit_label_ref(label, s);
  s << endl;
}

static void emit_bgti(const char *src1, int imm, int label, ostream &s)
{
  s << BGT << src1 << " " << imm << " ";
  emit_label_ref(label, s);
  s << endl;
}

static void emit_branch(int l, ostream &s)
{
  s << BRANCH;
  emit_label_ref(l, s);
  s << endl;
}

//
// Push a register on the stack. The stack grows towards smaller addresses.
//
static void emit_push(const char *reg, ostream &str)
{
  emit_store(reg, 0, SP, str);
  emit_addiu(SP, SP, -4, str);
}

//
// Fetch the integer value in an Int object.
// Emits code to fetch the integer value of the Integer object pointed
// to by register source into the register dest
//
static void emit_fetch_int(const char *dest, const char *source, ostream &s)
{
  emit_load(dest, DEFAULT_OBJFIELDS, source, s);
}

//
// Emits code to store the integer value contained in register source
// into the Integer object pointed to by dest.
//
static void emit_store_int(const char *source, const char *dest, ostream &s)
{
  emit_store(source, DEFAULT_OBJFIELDS, dest, s);
}

static void emit_test_collector(ostream &s)
{
  emit_push(ACC, s);
  emit_move(ACC, SP, s);  // stack end
  emit_move(A1, ZERO, s); // allocate nothing
  s << JAL << gc_collect_names[cgen_Memmgr] << endl;
  emit_addiu(SP, SP, 4, s);
  emit_load(ACC, 0, SP, s);
}

static void emit_gc_check(const char *source, ostream &s)
{
  if (source != (char *)A1)
    emit_move(A1, source, s);
  s << JAL << "_gc_check" << endl;
}

static void emit_callee_prologue(ostream &s) {
    // Reserve stack space to backup $fp, $s0, $ra
    emit_addiu(SP, SP, -12, s);

    emit_store(FP, 3, SP, s);
    emit_store(SELF, 2, SP, s);
    emit_store(RA, 1, SP, s);
    emit_addiu(FP, SP, 4, s);

    // Move current object to $s0
    emit_move(SELF, ACC, s);
}

static void emit_callee_epilogue(int param_count, ostream &s) {
    // Load the return address to $ra, prepare to return
    emit_load(RA, 1, SP, s);
    emit_load(SELF, 2, SP, s);
    emit_load(FP, 3, SP, s);
    emit_addiu(SP, SP, (3 + param_count) * WORD_SIZE, s);
    emit_return(s);
}

static void emit_default_value_for_types(Symbol type_name, ostream &s) {
  if (type_name == Str) {
    StringEntry *p = stringtable.lookup_string("");

    emit_partial_load_address(ACC, s);
    p->code_ref(s);
    s << std::endl;

  } else if (type_name == Int) {
    IntEntry *p = inttable.lookup_string("0");
    
    emit_partial_load_address(ACC, s);
    p->code_ref(s);
    s << std::endl;
  } else if (type_name == Bool) {
    emit_load_bool(ACC, BoolConst(0), s);
  } else {
    emit_move(ACC, ZERO, s);
  }
}

/*
  Params:
    node : Used to get line number message info
    label_index : label_index to jump to when ACC is not 0
    failure_routine : Routine to call when $a0 is 0
*/
static void emit_abort_on_acc_void(
    tree_node *treenode,
    int l_index, 
    const char *failure_routine, 
    ostream &s, Environment *env) {

  CgenNode *node = env->get_current_class();
  assert(node);

  StringEntry *file_name_entry = (StringEntry *)node->get_filename();

  // Handle void object in which case we need to call _dispatch_abort
  emit_bne(ACC, ZERO, l_index, s);

  emit_load_imm(T1, treenode->get_line_number(), s);
  emit_load_string(ACC, file_name_entry, s);
  emit_jal(failure_routine, s);
}

static void emit_case_abort(
    const char *failure_routine, 
    ostream &s, Environment *env) {

  CgenNode *node = env->get_current_class();
  assert(node);

  StringEntry *file_name_entry = (StringEntry *)node->get_filename();

  // Handle void object in which case we need to call _dispatch_abort
  emit_load_string(ACC, file_name_entry, s);
  emit_jal(failure_routine, s);
}


///////////////////////////////////////////////////////////////////////////////
//
// coding strings, ints, and booleans
//
// Cool has three kinds of constants: strings, ints, and booleans.
// This section defines code generation for each type.
//
// All string constants are listed in the global "stringtable" and have
// type StringEntry.  StringEntry methods are defined both for String
// constant definitions and references.
//
// All integer constants are listed in the global "inttable" and have
// type IntEntry.  IntEntry methods are defined for Int
// constant definitions and references.
//
// Since there are only two Bool values, there is no need for a table.
// The two booleans are represented by instances of the class BoolConst,
// which defines the definition and reference methods for Bools.
//
///////////////////////////////////////////////////////////////////////////////

//
// Strings
//
void StringEntry::code_ref(ostream &s)
{
  s << STRCONST_PREFIX << index;
}

//
// Emit code for a constant String.
// You should fill in the code naming the dispatch table.
//

void StringEntry::code_def(ostream &s, int stringclasstag)
{
  IntEntryP lensym = inttable.add_int(len);

  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);
  s << LABEL                                                              // label
    << WORD << stringclasstag << endl                                     // tag
    << WORD << (DEFAULT_OBJFIELDS + STRING_SLOTS + (len + 4) / 4) << endl // size
    << WORD << STRINGNAME << DISPTAB_SUFFIX;

  s << endl; // dispatch table
  s << WORD;
  lensym->code_ref(s);
  s << endl;                    // string length
  emit_string_constant(s, str); // ascii string
  s << ALIGN;                   // align to word
}

//
// StrTable::code_string
// Generate a string object definition for every string constant in the
// stringtable.
//
void StrTable::code_string_table(ostream &s, int stringclasstag)
{
  for (List<StringEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s, stringclasstag);
}

//
// Ints
//
void IntEntry::code_ref(ostream &s)
{
  s << INTCONST_PREFIX << index;
}

//
// Emit code for a constant Integer.
// You should fill in the code naming the dispatch table.
//

void IntEntry::code_def(ostream &s, int intclasstag)
{
  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);
  s << LABEL                                           // label
    << WORD << intclasstag << endl                     // class tag
    << WORD << (DEFAULT_OBJFIELDS + INT_SLOTS) << endl // object size
    << WORD << INTNAME << DISPTAB_SUFFIX;

  s << endl;                // dispatch table
  s << WORD << str << endl; // integer value
}

//
// IntTable::code_string_table
// Generate an Int object definition for every Int constant in the
// inttable.
//
void IntTable::code_string_table(ostream &s, int intclasstag)
{
  for (List<IntEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s, intclasstag);
}

//
// Bools
//
BoolConst::BoolConst(int i) : val(i) { assert(i == 0 || i == 1); }

void BoolConst::code_ref(ostream &s) const
{
  s << BOOLCONST_PREFIX << val;
}

//
// Emit code for a constant Bool.
// You should fill in the code naming the dispatch table.
//

void BoolConst::code_def(ostream &s, int boolclasstag)
{
  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);
  s << LABEL                                            // label
    << WORD << boolclasstag << endl                     // class tag
    << WORD << (DEFAULT_OBJFIELDS + BOOL_SLOTS) << endl // object size
    << WORD << BOOLNAME << DISPTAB_SUFFIX;

  /***** Add dispatch information for class Bool ******/

  s << endl;                // dispatch table
  s << WORD << val << endl; // value (0 or 1)
}

//////////////////////////////////////////////////////////////////////////////
//
//  CgenClassTable methods
//
//////////////////////////////////////////////////////////////////////////////

//***************************************************
//
//  Emit code to start the .data segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_data()
{
  Symbol main = idtable.lookup_string(MAINNAME);
  Symbol string = idtable.lookup_string(STRINGNAME);
  Symbol integer = idtable.lookup_string(INTNAME);
  Symbol boolc = idtable.lookup_string(BOOLNAME);

  str << "\t.data\n"
      << ALIGN;
  //
  // The following global names must be defined first.
  //
  str << GLOBAL << CLASSNAMETAB << endl;
  str << GLOBAL;
  emit_protobj_ref(main, str);
  str << endl;
  str << GLOBAL;
  emit_protobj_ref(integer, str);
  str << endl;
  str << GLOBAL;
  emit_protobj_ref(string, str);
  str << endl;
  str << GLOBAL;
  falsebool.code_ref(str);
  str << endl;
  str << GLOBAL;
  truebool.code_ref(str);
  str << endl;
  str << GLOBAL << INTTAG << endl;
  str << GLOBAL << BOOLTAG << endl;
  str << GLOBAL << STRINGTAG << endl;

  //
  // We also need to know the tag of the Int, String, and Bool classes
  // during code generation.
  //
  str << INTTAG << LABEL
      << WORD << intclasstag << endl;
  str << BOOLTAG << LABEL
      << WORD << boolclasstag << endl;
  str << STRINGTAG << LABEL
      << WORD << stringclasstag << endl;
}

//***************************************************
//
//  Emit code to start the .text segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_text()
{
  str << GLOBAL << HEAP_START << endl
      << HEAP_START << LABEL
      << WORD << 0 << endl
      << "\t.text" << endl
      << GLOBAL;
  emit_init_ref(idtable.add_string("Main"), str);
  str << endl
      << GLOBAL;
  emit_init_ref(idtable.add_string("Int"), str);
  str << endl
      << GLOBAL;
  emit_init_ref(idtable.add_string("String"), str);
  str << endl
      << GLOBAL;
  emit_init_ref(idtable.add_string("Bool"), str);
  str << endl
      << GLOBAL;
  emit_method_ref(idtable.add_string("Main"), idtable.add_string("main"), str);
  str << endl;
}

void CgenClassTable::code_bools(int boolclasstag)
{
  falsebool.code_def(str, boolclasstag);
  truebool.code_def(str, boolclasstag);
}

void CgenClassTable::code_select_gc()
{
  //
  // Generate GC choice constants (pointers to GC functions)
  //
  str << GLOBAL << "_MemMgr_INITIALIZER" << endl;
  str << "_MemMgr_INITIALIZER:" << endl;
  str << WORD << gc_init_names[cgen_Memmgr] << endl;
  str << GLOBAL << "_MemMgr_COLLECTOR" << endl;
  str << "_MemMgr_COLLECTOR:" << endl;
  str << WORD << gc_collect_names[cgen_Memmgr] << endl;
  str << GLOBAL << "_MemMgr_TEST" << endl;
  str << "_MemMgr_TEST:" << endl;
  str << WORD << (cgen_Memmgr_Test == GC_TEST) << endl;
}

//********************************************************
//
// Emit code to reserve space for and initialize all of
// the constants.  Class names should have been added to
// the string table (in the supplied code, is is done
// during the construction of the inheritance graph), and
// code for emitting string constants as a side effect adds
// the string's length to the integer table.  The constants
// are emmitted by running through the stringtable and inttable
// and producing code for each entry.
//
//********************************************************

void CgenClassTable::code_constants()
{
  //
  // Add constants that are required by the code generator.
  //
  stringtable.add_string("");
  inttable.add_string("0");

  stringtable.code_string_table(str, stringclasstag);
  inttable.code_string_table(str, intclasstag);
  code_bools(boolclasstag);
}

void CgenClassTable::set_class_tags()
{
  // Here we set class tag in pre-order traversal order. The reason we
  // use this order is that in this way, a class type's all decendents's tags
  // are continuous in the range (node_tag, right_most_child_tag]. This way
  // we can generate efficient code for the typcase expression which depends
  // on object's runtime type information to decide which branch to choose.

  // This is called after inheritance tree is built.
  int tag = 0;
  CgenNode *p = root();

  p->set_tag_recursive(tag, ordered_classes_);
}

CgenClassTable::CgenClassTable(Classes classes, ostream &s) : nds(NULL), str(s)
{
  stringclasstag = 0 /* Change to your String class tag here */;
  intclasstag = 0 /* Change to your Int class tag here */;
  boolclasstag = 0 /* Change to your Bool class tag here */;

  enterscope();
  if (cgen_debug)
    cout << "Building CgenClassTable" << endl;
  install_basic_classes();
  install_classes(classes);
  build_inheritance_tree();

  set_class_tags();

  stringclasstag = lookup(Str)->get_tag();
  intclasstag = lookup(Int)->get_tag();
  boolclasstag = lookup(Bool)->get_tag();

  code();
  exitscope();
}

void CgenClassTable::install_basic_classes()
{

  // The tree package uses these globals to annotate the classes built below.
  // curr_lineno  = 0;
  Symbol filename = stringtable.add_string("<basic class>");

  //
  // A few special class names are installed in the lookup table but not
  // the class list.  Thus, these classes exist, but are not part of the
  // inheritance hierarchy.
  // No_class serves as the parent of Object and the other special classes.
  // SELF_TYPE is the self class; it cannot be redefined or inherited.
  // prim_slot is a class known to the code generator.
  //
  addid(No_class,
        new CgenNode(class_(No_class, No_class, nil_Features(), filename),
                     Basic, this));
  addid(SELF_TYPE,
        new CgenNode(class_(SELF_TYPE, No_class, nil_Features(), filename),
                     Basic, this));
  addid(prim_slot,
        new CgenNode(class_(prim_slot, No_class, nil_Features(), filename),
                     Basic, this));

  //
  // The Object class has no parent class. Its methods are
  //        cool_abort() : Object    aborts the program
  //        type_name() : Str        returns a string representation of class name
  //        copy() : SELF_TYPE       returns a copy of the object
  //
  // There is no need for method bodies in the basic classes---these
  // are already built in to the runtime system.
  //
  install_class(
      new CgenNode(
          class_(Object,
                 No_class,
                 append_Features(
                     append_Features(
                         single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
                         single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
                     single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
                 filename),
          Basic, this));

  //
  // The IO class inherits from Object. Its methods are
  //        out_string(Str) : SELF_TYPE          writes a string to the output
  //        out_int(Int) : SELF_TYPE               "    an int    "  "     "
  //        in_string() : Str                    reads a string from the input
  //        in_int() : Int                         "   an int     "  "     "
  //
  install_class(
      new CgenNode(
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
                 filename),
          Basic, this));

  //
  // The Int class has no methods and only a single attribute, the
  // "val" for the integer.
  //
  install_class(
      new CgenNode(
          class_(Int,
                 Object,
                 single_Features(attr(val, prim_slot, no_expr())),
                 filename),
          Basic, this));

  //
  // Bool also has only the "val" slot.
  //
  install_class(
      new CgenNode(
          class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())), filename),
          Basic, this));

  //
  // The class Str has a number of slots and operations:
  //       val                                  ???
  //       str_field                            the string itself
  //       length() : Int                       length of the string
  //       concat(arg: Str) : Str               string concatenation
  //       substr(arg: Int, arg2: Int): Str     substring
  //
  install_class(
      new CgenNode(
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
                 filename),
          Basic, this));
}

// CgenClassTable::install_class
// CgenClassTable::install_classes
//
// install_classes enters a list of classes in the symbol table.
//
void CgenClassTable::install_class(CgenNodeP nd)
{
  Symbol name = nd->get_name();

  if (probe(name))
  {
    return;
  }

  // The class name is legal, so add it to the list of classes
  // and the symbol table.
  nds = new List<CgenNode>(nd, nds);
  addid(name, nd);
}

void CgenClassTable::install_classes(Classes cs)
{
  for (int i = cs->first(); cs->more(i); i = cs->next(i))
    install_class(new CgenNode(cs->nth(i), NotBasic, this));
}

//
// CgenClassTable::build_inheritance_tree
//
void CgenClassTable::build_inheritance_tree()
{
  for (List<CgenNode> *l = nds; l; l = l->tl())
    set_relations(l->hd());
}

//
// CgenClassTable::set_relations
//
// Takes a CgenNode and locates its, and its parent's, inheritance nodes
// via the class table.  Parent and child pointers are added as appropriate.
//
void CgenClassTable::set_relations(CgenNodeP nd)
{
  CgenNode *parent_node = probe(nd->get_parent());
  nd->set_parentnd(parent_node);
  parent_node->add_child(nd);
}

void CgenClassTable::code_class_nametab()
{
  // Put these in the first three slots of name_table
  str << "\t.data" << std::endl;
  str << CLASSNAMETAB << LABEL;

  for (CgenNode *p : ordered_classes_)
  {
    p->code_nametab_entry(str);
  }

  // stream_name_table_ << WORD << lookup()

  // init name

  // IntEntryP lensym = inttable.add_int(len);

  // Add -1 eye catcher
  // s << WORD << "-1" << endl;

  // code_ref(s);
  // s << LABEL                                                              // label
  //   << WORD << stringclasstag << endl                                     // tag
  //   << WORD << (DEFAULT_OBJFIELDS + STRING_SLOTS + (len + 4) / 4) << endl // size
  //   << WORD;

  // /***** Add dispatch information for class String ******/

  // s << endl; // dispatch table
  // s << WORD;
  // lensym->code_ref(s);
  // s << endl;                    // string length
  // emit_string_constant(s, str); // ascii string
  // s << ALIGN;
}

void CgenClassTable::code_class_objtab()
{
  str << "\t.data" << std::endl;
  str << CLASSOBJTAB << LABEL;

  for (CgenNode *p : ordered_classes_)
  {
    p->code_objtab_entry(str);
  }
}

void CgenClassTable::code_class_protobjs()
{
  /*
    +0  : class_tag
    +4  : size
    +8  : dispatch_table
    +12 : attr0
    +16 : attr1
    ...
  */
  for (CgenNode *p : ordered_classes_) {
    p->collect_features();
  }

  for (CgenNode *p : ordered_classes_) {
    p->code_class_disptab(str);
    p->code_class_protobj(str);
  }
}

void CgenClassTable::code_class_methods() {

  Environment env(this);
  for (CgenNode *p : ordered_classes_) {
    Symbol sym_name = p->get_name();

    // runtime has already provide these
    if (sym_name == Object || sym_name == IO || sym_name == Str) {
      continue;
    }
    env.set_current_class(p);
    p->code_methods(str, &env);
  }
}

void CgenClassTable::code_class_initializers() {

    Environment env(this);
    for (CgenNode *p : ordered_classes_) {
      env.set_current_class(p);
      p->code_initializer(str, &env);
  }
}

void CgenClassTable::code()
{
  if (cgen_debug)
    cout << "coding global data" << endl;
  code_global_data();

  if (cgen_debug)
    cout << "choosing gc" << endl;
  code_select_gc();

  if (cgen_debug)
    cout << "coding constants" << endl;
  code_constants();

  code_class_nametab();

  code_class_objtab();

  code_class_protobjs();


  if (cgen_debug)
    cout << "coding global text" << endl;
  code_global_text();

  code_class_initializers();
  code_class_methods();
}

CgenNodeP CgenClassTable::root()
{
  return probe(Object);
}

///////////////////////////////////////////////////////////////////////
//
// CgenNode methods
//
///////////////////////////////////////////////////////////////////////

CgenNode::CgenNode(Class_ nd, Basicness bstatus, CgenClassTableP ct) : class__class((const class__class &)*nd),
                                                                       parentnd(NULL),
                                                                       children(NULL),
                                                                       basic_status(bstatus)
{
  stringtable.add_string(name->get_string()); // Add class name to string table
}

void CgenNode::add_child(CgenNodeP n)
{
  children = new List<CgenNode>(n, children);
}

void CgenNode::set_parentnd(CgenNodeP p)
{
  assert(parentnd == NULL);
  assert(p != NULL);
  parentnd = p;
}

void CgenNode::copy_features(
    std::unordered_map<Symbol, size_t> &attr_map,
    std::vector<AttrInfo> &attr_vec,
    std::unordered_map<Symbol, size_t> &method_map,
    std::vector<MethodInfo> &method_vec)
{
  attr_map = attr_map_;
  attr_vec = attr_vec_;
  method_map = method_map_;
  method_vec = method_vec_;
}

void CgenNode::collect_self_features()
{
  for (int i = features->first(); features->more(i); i = features->next(i)) {
    if (features->nth(i)->is_method()) {
      method_class *p = dynamic_cast<method_class *>(features->nth(i));

      auto iter = method_map_.find(p->name);
      // Existing method in parent class, override it
      if (iter != method_map_.end()) {
        method_vec_[iter->second] = {p->name, get_name(), p};
      } else {
        method_vec_.push_back({p->name, get_name(), p});
        method_map_[p->name] = method_vec_.size() - 1;
      }

    } else {
      attr_class *p = dynamic_cast<attr_class *>(features->nth(i));
      attr_vec_.push_back({p->name, p->type_decl, p});
      attr_map_[p->name] = {attr_vec_.size() - 1};
    }
  }
}

// Caller guarantees this routine is called in a right order of CgenNode.
// So the parent's features are already prepared.
void CgenNode::collect_features()
{
  // This function will be called on all real class types starting from Object.
  // It won't be called on No_class and similar dummy classes.
  if (get_name() != Object) {
    get_parentnd()->copy_features(attr_map_, attr_vec_, method_map_, method_vec_);
  }

  collect_self_features();
}

void CgenNode::set_tag_recursive(int &tag, std::vector<CgenNode *> &ordered_classes) {
      class_tag_ = tag++;

      ordered_classes.push_back(this);
      auto p = children;

      while (p)
      {
         CgenNode *node = p->hd();
         if (node) {
            node->set_tag_recursive(tag, ordered_classes);
         }
         p = p->tl();
      };

      max_child_tag_ = tag - 1;
}

// define protObj (Here we need to recursively add attrs from its ancestor)
// .word  tag
// .word  size
// .word  dispatch_table_addr
// .word  attr1
// .word  attr2
void CgenNode::code_class_protobj(ostream &str)
{
  // declare
  str << GLOBAL << get_name() << PROTOBJ_SUFFIX << std::endl;

  // define a word of -1 for GC?
  str << WORD << "-1" << std::endl;

  str << get_name() << PROTOBJ_SUFFIX << LABEL;
  str << WORD << get_tag() << std::endl;
  str << WORD << get_protobj_size() << std::endl;
  str << WORD << get_name() << DISPTAB_SUFFIX << std::endl;
  
  for (auto &attr_info : attr_vec_) {
    if (attr_info.type == Bool) {
      str << WORD << BOOLCONST_PREFIX << 0 << std::endl;

    } else if (attr_info.type == Int) {
      IntEntry *p = inttable.lookup_string("0");
      str << WORD;
      p->code_ref(str);
      str << std::endl;

    } else if (attr_info.type == Str) {
      StringEntry *p = stringtable.lookup_string("");
      str << WORD;
      p->code_ref(str);
      str << std::endl;
    } else {
      str << WORD << 0 << std::endl;
    }
  }
}

void CgenNode::code_class_disptab(ostream &str) {
  // declare
  // str << GLOBAL << disptab_name << "\n";

  // define a word of -1 for GC?
  str << WORD << "-1" << std::endl;

  str << get_name() << DISPTAB_SUFFIX << LABEL;

  for (auto info : method_vec_) {
    str << WORD << info.owner_name << METHOD_SEP << info.name << std::endl;
  }
}

void CgenNode::code_nametab_entry(ostream &s)
{
  /*
    +0  : class0_name_const_string
    +4  : class1_name_const_string
    +8  : class2_name_const_string
    +12 : class3_name_const_string
  */
  Symbol class_name = get_name();
  StringEntry *s_class_name = stringtable.lookup_string(class_name->get_string());

  assert(s_class_name);

  s << WORD;
  s_class_name->code_ref(s);
  s << std::endl;
}

void CgenNode::code_objtab_entry(ostream &s)
{
  /*
    +0  : class0_protObj
    +4  : class0_init
    +8  : class1_protObj
    +12 : class1_init
  */
  Symbol class_name_sym = get_name();
  std::string class_name = class_name_sym->get_string();

  s << WORD << get_name() << PROTOBJ_SUFFIX << std::endl;
  s << WORD << get_name() << CLASSINIT_SUFFIX << std::endl;
}

void CgenNode::code_initializer(ostream &s, Environment *env) {

  emit_init_ref(get_name(), s);
  s << LABEL;

  emit_callee_prologue(s);

  // Recursively call parent init first
  if (get_name() != Object) {
    emit_move(ACC, SELF, s);
    std::string parent_init = parent->get_string();
    parent_init += CLASSINIT_SUFFIX;
    emit_jal(parent_init.c_str(), s);
  }

  for (int i = features->first(); features->more(i); i = features->next(i)) {
    if (features->nth(i)->is_method())
      continue;

    attr_class *p = dynamic_cast<attr_class *>(features->nth(i));

    // If no init expression, the default values have already been set in xxx_protObj.
    // So we don't need to do extra work.
    if(p->init->is_empty()) {
      emit_default_value_for_types(p->type_decl, s);
    } else {
      // Execute the init expression. Result will be in ACC
      p->init->code(s, env);
    }

    int attr_index = get_attr_offset(p->name);
    emit_store(ACC, attr_index, SELF, s);

    // Do you need to move SELF to ACC on return?
    emit_move(ACC, SELF, s);
  }

  emit_callee_epilogue(0, s);
}
  
/*
  Callee save the $fp, $s0, $ra
*/
void CgenNode::code_methods(ostream &os, Environment *env) {
  Symbol name = get_name();

  for (int i = features->first(); features->more(i); i = features->next(i)) {
    if (!features->nth(i)->is_method())
      continue;

    env->enterscope();
    method_class *p =static_cast<method_class *>(features->nth(i));

    /*
    7.3 Stack and Register Conventions 
    The primitive methods in the runtime system expect arguments in register $a0 and on
    the stack. Usually $a0 contains the self object of the dispatch. Additional arguments
    should be on top of the stack, first argument pushed first (an issue only for String.
    substr, which takes two arguments). Some of the primitive runtime procedures expect
    arguments in particular registers.
    */
    for (size_t j = p->formals->first(); p->formals->more(j); j = p->formals->next(j)) {
      formal_class *param = static_cast<formal_class *>(p->formals->nth(j));
      env->add_param(param->name, param->type_decl, p->formals->len() - 1 - j);
    }

    // Emit method label.
    os << name << METHOD_SEP << p->name << ":" << std::endl;

    // For a three parameter function, the frame will be:
    //        -------
    //        arg_1
    //        -------
    //        arg_2
    //        -------
    //        arg_3
    //        -------
    //        old $fp
    //        -------
    //        old $s0
    //        -------
    // $fp ->  $ra
    //        -------
    // $sp ->     
    //        -------
    // alloc stack room to save original $fp, $s0, $ra
    // $s0 will be reused as self pointer
    // 
    emit_callee_prologue(os);

    // Emit the code for function body.
    p->expr->code(os, env);

    emit_callee_epilogue(p->formals->len(), os);

    env->exitscope();
  }
}

//******************************************************************
//
//   Fill in the following methods to produce code for the
//   appropriate expression.  You may add or remove parameters
//   as you wish, but if you do, remember to change the parameters
//   of the declarations in `cool-tree.h'  Sample code for
//   constant integers, strings, and booleans are provided.
//
//*****************************************************************

void assign_class::code(ostream &s, Environment *env)
{
  // $a0 will keep the result of expr after call
  expr->code(s, env);

  // name won't be self which is an error should be detected at semantic phase
  VarInfo *info = env->lookup(name);

  if (info) // arg or variable
  {
    if (info->is_param) {
      emit_store(ACC, info->index + 3, FP, s);
    } else {
      emit_store(ACC, -(info->index + 1), FP, s);
    }

    return;
  }

  // name is attribute of the class
  int index = env->get_cur_class_attr_offset(name);

  emit_store(ACC, index, SELF, s);
}

void static_dispatch_class::code(ostream &s, Environment *env)
{
  /*
  so, S1,E ⊢ e1 : v1,S2 
  so, S2,E ⊢ e2 : v2,S3 
  . . . 
  so, Sn,E ⊢ en : vn,Sn+1 
  so, Sn+1,E ⊢ e0 : v0,Sn+2 
  v0 = X(a1 =la1 ,...,am = lam ) 
  implementation(T,f) = (x1,...,xn,en+1) 
  lxi = newloc(Sn+2), for i = 1...n and each lxi is distinct 
  Sn+3 = Sn+2[v1/lx1 ,...,vn/lxn ] 
  v0, Sn+3,[a1 : la1 ,...,am : lam , x1 : lx1 ,...,xn : lxn ] ⊢ en+1 : vn+1,Sn+4 
  -------------------------------------------------------------------------------
  so, S1,E ⊢ e0@T.f(e1,...,en) : vn+1,Sn+4
  */
  //  Expression expr;
  //  Symbol type_name;
  //  Symbol name;
  //  Expressions actual;

  // Reserve the stack space for all arguments since we need to push them in
  // reverse order.
  // emit_addiu(SP, SP, WORD_SIZE * actual->len(), s);

  for(int i = actual->first(); actual->more(i); i = actual->next(i)) {
    actual->nth(i)->code(s, env);
    emit_push(ACC, s);
    // emit_store(ACC, i + 1, SP, s);
  }

  expr->code(s, env); // ACC holds the object

  int next_label = label_index++;
  emit_abort_on_acc_void(this, next_label, "_dispatch_abort", s, env);

  // Note: We can't simply concatnate type_name.name to call the target method because
  // the typename itself may not contain the method name. Instead, it inherits it from
  // parent type. So this static way won't work in that case.

  emit_label_def(next_label, s);

  std::string type_name_s = type_name->get_string();

  // Load type_name's protObj address to T1
  emit_load_address(T1, (type_name_s + PROTOBJ_SUFFIX).c_str(), s);
  emit_load(T1, kDispTabOffset, T1, s);

  int method_index = env->get_method_index(type_name, name);
  emit_load(T2, method_index, T1, s);
  emit_jalr(T2, s);  
}

void dispatch_class::code(ostream &s, Environment *env)
{
  /*
  so, S1,E ⊢ e1 : v1,S2 
  so, S2,E ⊢ e2 : v2,S3 
  . . . 
  so, Sn,E ⊢ en : vn,Sn+1 
  so, Sn+1,E ⊢ e0 : v0,Sn+2 
  v0 = X(a1 =la1 ,...,am = lam ) 
  implementation(X,f) = (x1,...,xn,en+1) 
  lxi = newloc(Sn+2), for i = 1...n and each lxi is distinct 
  Sn+3 = Sn+2[v1/lx1 ,...,vn/lxn ] 
  v0, Sn+3,[a1 : la1 ,...,am : lam , x1 : lx1 ,...,xn : lxn ] ⊢ en+1 : vn+1,Sn+4 
  ------------------------------------------------------------------------------
  so, S1,E ⊢ e0.f(e1,...,en) : vn+1,Sn+4
  */

  //  Expression expr;
  //  Symbol name;
  //  Expressions actual;

  // Reserve the stack space for all arguments since we need to push them in
  // reverse order.
  // emit_addiu(SP, SP, WORD_SIZE * actual->len(), s);

  for(int i = actual->first(); actual->more(i); i = actual->next(i)) {
    actual->nth(i)->code(s, env);
    // emit_store(ACC, i + 1, SP, s);
    emit_push(ACC, s);
  }

  expr->code(s, env);

  // For self(SELF_TYPE), compiler is not able to get its class name at compile time.
  // You may think env->get_current_class()->get_name() is its type name at first glance.
  // However, that's not the case. Consider calling a child method from parent class, for
  // example:
  //
  // class A { 
  //     test_1() : Int {
  //         1
  //     };  

  //     test_2() : Int {
  //         self.test_1()
  //     };  
  // };
  // class B inherits A { 
  //     test_1() : Int {
  //         22
  //     };  
  // };
  //
  // class Main inherits IO {
  //    main(): SELF_TYPE {
  //     let x : B <- new B in {
  //         out_int(x.test_2());
  //     }   
  //    };  
  // };

  int next_label = label_index++;
  emit_abort_on_acc_void(this, next_label, "_dispatch_abort", s, env);

  // Get object's method address: 
  //  1. expr -> type_sym -> CgenNode -> method_index 
  //  2. dispTab[method_index]

  // Get dispTab
  emit_label_def(next_label, s);
  emit_load(T1, 2, ACC, s);

  Symbol obj_type = expr->get_type();
  if (obj_type == SELF_TYPE) {
    obj_type = env->get_current_class()->get_name();
  }
  size_t method_index = env->get_class_cgen_node(obj_type)->get_method_index(name);

  // Get method address
  emit_load(T2, method_index, T1, s);

  emit_jalr(T2, s);
}

void cond_class::code(ostream &s, Environment *env)
{
  /*
    so, S1,E ⊢ e1 : Bool(false),S2 
    so, S2,E ⊢ e3 : v3,S3
    ------------------------------------------- 
    so, S1,E ⊢ if e1 then e2 else e3 fi : v3,S3
  */
  /*
    so, S1,E ⊢ e1 : Bool(true),S2 
    so, S2,E ⊢ e2 : v2,S3 
    -------------------------------------------
    so, S1,E ⊢ if e1 then e2 else e3 fi : v2,S3
  */

  //  Expression pred;
  //  Expression then_exp;
  //  Expression else_exp;

  pred->code(s, env);

  emit_load(T1, kFirstValueOffset, ACC, s);

  int false_label_index = label_index++;

  emit_beqz(T1, false_label_index, s);

  // True branch
  then_exp->code(s, env);

  // Jump to the end.
  int end_label_index = label_index++;
  emit_branch(end_label_index, s);

  emit_label_def(false_label_index, s);

  else_exp->code(s, env);

  emit_label_def(end_label_index, s);
}

void loop_class::code(ostream &s, Environment *env)
{
  /*
  so, S1,E ⊢ e1 : Bool(true),S2 
  so, S2,E ⊢ e2 : v2,S3 
  so, S3,E ⊢ while e1 loop e2 pool : void,S4 
  ------------------------------------------
  so, S1,E ⊢ while e1 loop e2 pool : void,S4


  so, S1,E ⊢ e1 : Bool(false),S2 
  ------------------------------------------
  so, S1,E ⊢ while e1 loop e2 pool : void,S2
  */

  //  Expression pred;
  //  Expression body;

  int loop_start = label_index++;
  emit_label_def(loop_start, s);
  pred->code(s, env);

  // check the result of Bool result
  emit_load(T1, 3, ACC, s);

  int loop_end = label_index++;
  emit_beqz(T1, loop_end, s);

  body->code(s, env);

  emit_branch(loop_start, s);
  emit_label_def(loop_end, s);
  emit_move(ACC, ZERO, s);
}

void typcase_class::code(ostream &s, Environment *env)
{
  /*
  so, S1,E ⊢ e0 : v0,S2 
  v0 = X(...) 
  Ti = closest ancestor of X in {T1,...,Tn} 
  l0 = newloc(S2) 
  S3 =S2[v0/l0] 
  E′ =E[l0/Idi] 
  so, S3,E′ ⊢ ei : v1,S4 [Case] 
  -------------------------------------------------------------------
  so, S1,E ⊢ case e0 of Id1 : T1 ⇒ e1;...;Idn : Tn ⇒ en; esac : v1,S4
  */
  //  Expression expr;
  //  Cases cases;

  expr->code(s, env);

  Symbol type_name = expr->get_type();
  if (type_name == SELF_TYPE) {
    type_name = env->get_current_class()->get_name();
  }

  int next_label = label_index++;
  emit_abort_on_acc_void(this, next_label, "_case_abort2", s, env);

  emit_label_def(next_label, s);

  // Load class_tag to T1
  emit_load(T1, 0, ACC, s);

  struct branch_node {
    branch_class *branch;
    int class_tag;
    int max_child_class_tag;

    branch_node(branch_class *p, Environment *env) : branch(p) {
      // SELF_TYPE is not allow in branch's type_decl
      CgenNode *node = env->get_class_cgen_node(p->type_decl);
      assert(node);
      class_tag = node->get_tag();
      max_child_class_tag = node->get_max_child_tag();
    }
  };

  auto compare = [](const branch_node &lhs, const branch_node &rhs) {
      return lhs.class_tag < rhs.class_tag;
  };

  std::priority_queue<branch_node, std::vector<branch_node>, decltype(compare)> pq(compare);

  for (int i = cases->first(); cases->more(i); i = cases->next(i)) {
    branch_class *branch = dynamic_cast<branch_class *>(cases->nth(i));
    //  Symbol name;
    //  Symbol type_decl;
    //  Expression expr;
    pq.push(branch_node(branch, env));
  }

  int case_end_index = label_index++;
  while (!pq.empty()) {
    const branch_node &node = pq.top();

    int next_index = label_index++;
    emit_blti(T1, node.class_tag, next_index, s);
    emit_bgti(T1, node.max_child_class_tag, next_index, s);
    
    env->enterscope();

    // Bound case expr to branch var
    env->add_var(node.branch->name, node.branch->type_decl);
    emit_push(ACC, s);
    node.branch->expr->code(s, env);

    emit_addiu(SP, SP, 4 * env->get_scope_var_count(), s);
    env->exitscope();

    emit_branch(case_end_index, s);
    emit_label_def(next_index, s);

    pq.pop();
  }
  // default no match handler. The object still in ACC
  emit_jal("_case_abort", s);
  emit_label_def(case_end_index, s);

}

void block_class::code(ostream &s, Environment *env)
{
  /*
    so, S1,E ⊢ e1 : v1,S2 
    so, S2,E ⊢ e2 : v2,S3 
    . . . so, Sn,E ⊢ en : vn,Sn+1 
    ---------------------------------------
    so, S1,E ⊢ { e1;e2;...;en; } : vn,Sn+1
  */

  for (int i = body->first(); body->more(i) ; i = body->next(i))
  {
    body->nth(i)->code(s, env);
  }
}

void let_class::code(ostream &s, Environment *env)
{
/*
  so, S1,E ⊢ e1 : v1,S2 
  l1 = newloc(S2) 
  S3 =S2[v1/l1] 
  E′=E[l1/Id] 
  so, S3,E′ ⊢ e2 : v2,S4
  -----------------------------------------
  so, S1,E ⊢ let Id : T1 ← e1 in e2 : v2,S4
*/

  //  Symbol identifier;
  //  Symbol type_decl;
  //  Expression init;
  //  Expression body;

  env->enterscope();

  // For default values, init->code with return void. However, we have to
  // handle String, Int and Bool separately according to the Cool manual.
  if (init->is_empty())
  {
    emit_default_value_for_types(type_decl, s);
  } else {
    init->code(s, env);
  }

  env->add_var(identifier, type_decl);

  // Assign init result to identifier
  emit_push(ACC, s);

  body->code(s, env);

  // Restore the stack to get rid of the variables at current scope
  // For current Cool syntax, env->get_scope_var_count() is always 1.
  emit_addiu(SP, SP, 4 * env->get_scope_var_count(), s);

  env->exitscope();
}

void plus_class::code(ostream &s, Environment *env)
{
  e1->code(s, env);

  // Create a copy for the dividend which will be used as a result
  emit_jal("Object.copy", s);
  emit_push(ACC, s);

  e2->code(s, env);

  emit_load(T1, 1, SP, s);
  emit_addiu(SP, SP, 4, s);

  // Load the int value of the dividend and divisor
  emit_load(T2, 3, T1, s);
  emit_load(T3, 3, ACC, s);

  // Do the division and save result to T2
  emit_add(T2, T2, T3, s);

  // Store T2 to T1's Int constant
  emit_store(T2, 3, T1, s);

  emit_move(ACC, T1, s);
}

void sub_class::code(ostream &s, Environment *env)
{
  e1->code(s, env);

  // Create a copy for the dividend which will be used as a result
  emit_jal("Object.copy", s);
  emit_push(ACC, s);

  e2->code(s, env);

  emit_load(T1, 1, SP, s);
  emit_addiu(SP, SP, 4, s);

  // Load the int value of the dividend and divisor
  emit_load(T2, 3, T1, s);
  emit_load(T3, 3, ACC, s);

  // Do the division and save result to T2
  emit_sub(T2, T2, T3, s);

  // Store T2 to T1's Int constant
  emit_store(T2, 3, T1, s);

  emit_move(ACC, T1, s);
}

void mul_class::code(ostream &s, Environment *env)
{
  e1->code(s, env);

  // Create a copy for the dividend which will be used as a result
  emit_jal("Object.copy", s);
  emit_push(ACC, s);

  e2->code(s, env);

  emit_load(T1, 1, SP, s);
  emit_addiu(SP, SP, 4, s);

  // Load the int value of the dividend and divisor
  emit_load(T2, 3, T1, s);
  emit_load(T3, 3, ACC, s);

  // Do the division and save result to T2
  emit_mul(T2, T2, T3, s);

  // Store T2 to T1's Int constant
  emit_store(T2, 3, T1, s);

  emit_move(ACC, T1, s);
}

void divide_class::code(ostream &s, Environment *env)
{
  e1->code(s, env);

  // Create a copy for the dividend which will be used as a result
  emit_jal("Object.copy", s);
  emit_push(ACC, s);

  e2->code(s, env);

  emit_load(T1, 1, SP, s);
  emit_addiu(SP, SP, 4, s);

  // Load the int value of the dividend and divisor
  emit_load(T2, 3, T1, s);
  emit_load(T3, 3, ACC, s);

  // Do the division and save result to T2
  emit_div(T2, T2, T3, s);

  // Store T2 to T1's Int constant
  emit_store(T2, 3, T1, s);

  emit_move(ACC, T1, s);
}

void neg_class::code(ostream &s, Environment *env)
{
  e1->code(s, env);

  emit_jal("Object.copy", s);

  // Get the int value
  emit_load(T1, 3, ACC, s);

  // Negate the value
  emit_neg(T1, T1, s);

  // Store the negtive value
  emit_store(T1, 3, ACC, s);
}

// Only int allowed
void lt_class::code(ostream &s, Environment *env)
{
  e1->code(s, env);

  emit_push(ACC, s);
  e2->code(s, env);

  // Move e2 result to T2 (ACC will be used as bool result)
  emit_move(T2, ACC, s);

  // Load e1 result
  emit_load(T1, 1, SP, s);
  emit_addiu(SP, SP, 4, s);

  emit_load_bool(ACC, BoolConst(1), s);

  emit_load(T1, 3, T1, s);
  emit_load(T2, 3, T2, s);

  int label_next = label_index++;
  emit_blt(T1, T2, label_next, s);
  emit_load_bool(ACC, BoolConst(0), s);
  emit_label_def(label_next, s);
}

void eq_class::code(ostream &s, Environment *env)
{
  e1->code(s, env);

  emit_push(ACC, s);
  e2->code(s, env);

  // Move e2 result to T2 (ACC will be used as bool result)
  emit_move(T2, ACC, s);

  // Load e1 result
  emit_load(T1, 1, SP, s);
  emit_addiu(SP, SP, 4, s);

  // Since type checking phase already guarantee the eq_class's operants must
  // have the same type for String/Int/Bool, we only need to check one.
  if (e1->get_type() == Str) {
    emit_load(T3, kStrLenOffset, T1, s);
    emit_load(T4, kStrLenOffset, T2, s);

    int label_end = label_index++;
    emit_load_bool(ACC, BoolConst(0), s);

    // We also need to check the length int constant's value instead of address.
    // Since for some dynamically generated string (eg. through String.substr)
    // the same Int constant will be separate int constant object. 
    emit_load(T3, kFirstValueOffset, T3, s);
    emit_load(T4, kFirstValueOffset, T4, s);
    emit_bne(T3, T4, label_end, s);

    emit_addiu(T1, T1, kStrCharOffset * WORD_SIZE, s);
    emit_addiu(T2, T2, kStrCharOffset * WORD_SIZE, s);

    // Loop compare the asciiz. For simplicity, we compare them by bytes.
    // We can improve the efficiency by comparing the string by word size
    // on the first part and by bytes on the leftover part.
    int label_loop = label_index++;
    
    emit_label_def(label_loop, s);
    emit_load_byte(T3, 0, T1, s);
    emit_load_byte(T4, 0, T2, s);

    emit_bne(T3, T4, label_end, s);
    emit_addiu(T1, T1, 1, s);
    emit_addiu(T2, T2, 1, s);

    emit_bne(T3, ZERO, label_loop, s);
    emit_load_bool(ACC, BoolConst(1), s);

    emit_label_def(label_end, s);

  } else if (e1->get_type() == Bool || e1->get_type() == Int) {
    emit_load(T1, 3, T1, s);
    emit_load(T2, 3, T2, s);

    int label_next = label_index++;
    emit_load_bool(ACC, BoolConst(1), s);
    emit_beq(T1, T2, label_next, s);
    emit_load_bool(ACC, BoolConst(0), s);
    emit_label_def(label_next, s);

  } else {
    int label_next = label_index++;
    emit_load_bool(ACC, BoolConst(1), s);
    emit_beq(T1, T2, label_next, s);
    emit_load_bool(ACC, BoolConst(0), s);
    emit_label_def(label_next, s);
  }
}

// Only int allowed
void leq_class::code(ostream &s, Environment *env)
{
  e1->code(s, env);

  emit_push(ACC, s);
  e2->code(s, env);

  // Move e2 result to T2 (ACC will be used as bool result)
  emit_move(T2, ACC, s);

  // Load e1 result
  emit_load(T1, 1, SP, s);
  emit_addiu(SP, SP, 4, s);

  emit_load_bool(ACC, BoolConst(1), s);

  emit_load(T1, 3, T1, s);
  emit_load(T2, 3, T2, s);
  
  emit_bleq(T1, T2, label_index, s);
  emit_load_bool(ACC, BoolConst(0), s);
  emit_label_def(label_index, s);

  label_index++;

}

// Bool not
void comp_class::code(ostream &s, Environment *env)
{
  e1->code(s, env);

  // Load the value in Bool object
  emit_load(T1, 3, ACC, s);
  emit_load_bool(ACC, BoolConst(1), s);

  emit_beqz(T1, label_index, s);
  emit_load_bool(ACC, BoolConst(0), s);

  emit_label_def(label_index, s);

  label_index++;
}

void int_const_class::code(ostream &s, Environment *env)
{
  //
  // Need to be sure we have an IntEntry *, not an arbitrary Symbol
  //
  emit_load_int(ACC, inttable.lookup_string(token->get_string()), s);
}

void string_const_class::code(ostream &s, Environment *env)
{
  emit_load_string(ACC, stringtable.lookup_string(token->get_string()), s);
}

void bool_const_class::code(ostream &s, Environment *env)
{
  emit_load_bool(ACC, BoolConst(val), s);
}

void new__class::code(ostream &s, Environment *env)
{
  std::string type_name_str;

  if (type_name != SELF_TYPE) {
    type_name_str = type_name->get_string();

    // move protObj to $a0
    emit_load_address(ACC, (type_name_str + PROTOBJ_SUFFIX).c_str(), s);

    // call Object.copy
    emit_jal("Object.copy", s);

    // Call init for the new object
    emit_jal((type_name_str + CLASSINIT_SUFFIX).c_str(), s);

    return;
  }

  // We can't just get the classname from the current processing class and regard it as the
  // target class because if the current method is in the parent class and it is called from
  // a inherited class then the SELF_TYPE should be the child class while the get_current_class()
  // will return the parent class.

  // Load the class_tag to $t1
  emit_load(T1, 0, SELF, s);

  // T2 class_objTab base address
  emit_load_address(T2, "class_objTab", s);

  // T1 = T1 * 8
  emit_sll(T1, T1, 3, s);

  // T1 is protObj entry for the item
  emit_addu(T1, T2, T1, s);

  // Load the protObj address
  emit_load(ACC, 0, T1, s);

  // Save the address in T1 before call a method which may modify T1
  emit_push(T1, s);

  // Copy the object
  emit_jal("Object.copy", s);

  // Restore the T1 which points to item of xxx_protObj and xxx_init in class_objTbl
  emit_load(T1, 1, SP, s);
  emit_addiu(SP, SP, 4, s);

  // Get the class init method
  emit_load(T1, 1, T1, s);

  // Call the init method
  emit_jalr(T1, s);
}

void isvoid_class::code(ostream &s, Environment *env)
{
  e1->code(s, env);

  emit_move(T1, ACC, s);
  emit_load_bool(ACC, BoolConst(1), s);
  emit_beqz(T1, label_index, s);

  // We shouldn't move zero to ACC directly. Since all Int constants in Cool
  // are Object, we need to move Int constant 0 object to ACC.
  // emit_move(ACC, ZERO, s);
  emit_load_bool(ACC, BoolConst(0), s);
  emit_label_def(label_index, s);
  label_index++;
}

void no_expr_class::code(ostream &s, Environment *env)
{
  // Default value (void) for expression without init
  emit_move(ACC, ZERO, s);
}

void object_class::code(ostream &s, Environment *env)
{
  // Handle self
  if (name == self) {
    emit_move(ACC, SELF, s);
    return;
  } 

  // Handle arguments or local variables
  VarInfo *p = env->lookup(name);
  if (p) {
    if (p->is_param) {
      emit_load(ACC, p->index + 3, FP, s);
    } else {
      emit_load(ACC, -(p->index + 1), FP, s);
    }
    return;
  }

  // Handle attribute
  int index = env->get_cur_class_attr_offset(name);
  emit_load(ACC, index, SELF, s);
}
