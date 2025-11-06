# Test suite for ccc compiler
# This file defines all tests organized by category

# Basic expression and constant folding tests
EXPR_TESTS = \
	cfold.c \
	simpleexpr.c \
	with_add.c

# Declaration and type tests
DECL_TESTS = \
	decl.c \
	complex_types.c \
	up_to_struct.c \
	no_collision.c

# Preprocessor tests
CPP_TESTS = \
	macro.c \
	include.c \
	multistr.c \
	string.c \
	stringify.c \
	defined_test.c \
	ifdef_test.c \
	ifndef_test.c \
	undef_test.c

# K&R style function tests
KR_FUNC_TESTS = \
	kr_funcs.c \
	kr_minimal.c \
	kr_noparams.c \
	kr_oneparam.c \
	kr_multi.c \
	kr_progressive.c \
	kr_locals.c

# Modern function tests
FUNC_TESTS = \
	empty_func.c \
	simple_func.c \
	modern_func.c

# Statement tests
STMT_TESTS = \
	statements.c \
	statements_kr.c \
	simple_statements.c \
	conditional.c \
	conditional_false.c \
	conditional_simple.c \
	compound_ops.c \
	compound_precedence.c \
	do.c \
	cont.c \
	no_switch.c \
	simple_int.c \
	switch.c \
	sw2.c

# sizeof operator tests
SIZEOF_TESTS = \
	sizeof_basic.c \
	sizeof_bitfield.c \
	sizeof_const.c \
	sizeof_one.c \
	sizeof_struct.c \
	sizeof_test.c \
	sizeof_multiptr.c \
	sizeof_regular_var.c \
	sizeof_typedef.c \
	sizeof_typedef_simple.c \
	sizeof_typedef_test.c \
	sizeof_typedef_types.c \
	sizeof_typedef_var.c \
	sizeof_var.c

# Typedef tests
TYPEDEF_TESTS = \
	typedef.c \
	typedefs.c \
	simple_typedef_decl.c \
	struct_forward_test.c \
	typedef_array.c \
	typedef_array_only.c \
	typedef_array_use.c \
	typedef_copy.c \
	typedef_debug.c \
	typedef_enum_test.c \
	typedef_in_func_test.c \
	typedef_local.c \
	typedef_minimal.c \
	typedef_minimal2.c \
	typedef_ptr_only.c \
	typedef_ptr_use.c \
	typedef_scope_test.c \
	typedef_separate.c \
	typedef_simple.c \
	typedef_struct_test.c \
	typedef_test1.c \
	typedef_test2.c

# Local variable tests
LOCAL_TESTS = \
	local.c \
	local2.c \
	local3.c \
	local4.c \
	local5.c \
	local_vars.c \
	two_locals.c

# Scope tests
SCOPE_TESTS = \
	scopes.c

# Struct tests
STRUCT_TESTS = \
	struct.c

# Minimal/smoke tests
MINIMAL_TESTS = \
	minimal.c \
	100.c \
	t2.c

# Type cast tests
CAST_TESTS = \
	typecast.c \
	typecast_typedef.c

# String literal tests
STRING_TESTS = \
	string_literals.c \
	array_init.c

# Miscellaneous/regression tests
MISC_TESTS = \
	hang.c \
	more.c \
	partial.c \
	ptest.c \
	stat.c \
	init_var.c \
	init_var2.c

# All tests combined
ALL_TESTS = \
	$(EXPR_TESTS) \
	$(DECL_TESTS) \
	$(CPP_TESTS) \
	$(KR_FUNC_TESTS) \
	$(FUNC_TESTS) \
	$(STMT_TESTS) \
	$(SIZEOF_TESTS) \
	$(TYPEDEF_TESTS) \
	$(LOCAL_TESTS) \
	$(SCOPE_TESTS) \
	$(STRUCT_TESTS) \
	$(CAST_TESTS) \
	$(STRING_TESTS) \
	$(MINIMAL_TESTS) \
	$(MISC_TESTS)

# Main test targets
.PHONY: test tests
test:
	./runtest.sh $(VERBOSE) $(ALL_TESTS)

tests:
	./runtest.sh $(VERBOSE)

# Test groups for selective testing
.PHONY: test-expr test-decl test-cpp test-kr test-func test-stmt test-sizeof test-typedef test-cast test-string
test-expr:
	./runtest.sh $(VERBOSE) $(EXPR_TESTS)

test-decl:
	./runtest.sh $(VERBOSE) $(DECL_TESTS)

test-cpp:
	./runtest.sh $(VERBOSE) $(CPP_TESTS)

test-kr:
	./runtest.sh $(VERBOSE) $(KR_FUNC_TESTS)

test-func:
	./runtest.sh $(VERBOSE) $(FUNC_TESTS)

test-stmt:
	./runtest.sh $(VERBOSE) $(STMT_TESTS)

test-sizeof:
	./runtest.sh $(VERBOSE) $(SIZEOF_TESTS)

test-typedef:
	./runtest.sh $(VERBOSE) $(TYPEDEF_TESTS)

test-cast:
	./runtest.sh $(VERBOSE) $(CAST_TESTS)

test-string:
	./runtest.sh $(VERBOSE) $(STRING_TESTS)

# Valgrind testing
.PHONY: valgrind
valgrind:
	./runvalgrind.sh $(ALL_TESTS)

# Clean test artifacts
.PHONY: clean
clean:
	rm -f *.i
