//===-- GCCPlugin.cpp - testcase for High-level LLVM middleend interface --===//
//
// Copyright (C) 2017 Leslie Zhai <lesliezhai@llvm.org.cn>
// Copyright (C) 2011 Daniel Marjamäki <daniel.marjamaki@gmail.com>
// Copyright (C) 2005 to 2014  Chris Lattner, Duncan Sands et al.
//
// This file is part of DragonEgg.
//
// DragonEgg is free software; you can redistribute it and/or modify it under
// the terms of the GNU General Public License as published by the Free Software
// Foundation; either version 2, or (at your option) any later version.
//
// DragonEgg is distributed in the hope that it will be useful, but WITHOUT ANY
// WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
// A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along with
// DragonEgg; see the file COPYING.  If not, write to the Free Software
// Foundation, 51 Franklin Street, Suite 500, Boston, MA 02110-1335, USA.
//
//===----------------------------------------------------------------------===//
// This file tests the high-level LLVM middleend interface.
//===----------------------------------------------------------------------===//

#include "auto-host.h"
#include "config.h"
#include "system.h"
#include "gcc-plugin.h"
#include "coretypes.h"
#include "tree.h"
#include "tree-pass.h"
#include "intl.h"
#include "parse-tree.h"
#include "diagnostic.h"
#if (GCC_MAJOR > 4)
#include "context.h"
#endif

int plugin_is_GPL_compatible;

static unsigned int rtl_emit_function();

/// pass_rtl_emit_function - RTL pass that converts a function to LLVM IR.
#if (GCC_MAJOR < 5)
static struct rtl_opt_pass pass_rtl_emit_function = { {
  RTL_PASS, "rtl_emit_function",         /* name */
#if (GCC_MINOR > 7)
  OPTGROUP_NONE,                         /* optinfo_flags */
#endif
  NULL,                                  /* gate */
  rtl_emit_function,                     /* execute */
  NULL,                                  /* sub */
  NULL,                                  /* next */
  0,                                     /* static_pass_number */
  TV_NONE,                               /* tv_id */
  PROP_ssa | PROP_gimple_leh | PROP_cfg, /* properties_required */
  0,                                     /* properties_provided */
  PROP_ssa | PROP_trees,                 /* properties_destroyed */
  TODO_verify_ssa | TODO_verify_flow | TODO_verify_stmts, /* todo_flags_start */
  TODO_ggc_collect /* todo_flags_finish */
} };
#else
const pass_data pass_data_rtl_emit_function = {
  RTL_PASS,                              /* type */
  "rtl_emit_function",                   /* name */
  OPTGROUP_NONE,                         /* optinfo_flags */
  TV_NONE,                               /* tv_id */
  PROP_ssa | PROP_gimple_leh | PROP_cfg, /* properties_required */
  0,                                     /* properties_provided */
  PROP_ssa | PROP_trees,                 /* properties_destroyed */
  0,                                     /* todo_flags_start */
  0,                                     /* todo_flags_finish */
};

class pass_rtl_emit_function : public rtl_opt_pass {
public:
  pass_rtl_emit_function(gcc::context *ctxt)
      : rtl_opt_pass(pass_data_rtl_emit_function, ctxt) {
    printf("DEBUG: %s, line %d: %s: %s: static_pass_number %d\n",
            __FILE__, __LINE__, __PRETTY_FUNCTION__, flag_check_pointer_bounds
            ? "flag_check_pointer_bounds" : "!flag_check_pointer_bounds",
            static_pass_number);
  }

  opt_pass *clone() final override {
    printf("DEBUG: %s, line %d: %s\n", __FILE__, __LINE__, __PRETTY_FUNCTION__);
    return this;/*new pass_rtl_emit_function(m_ctxt);*/
  }

  bool gate (function *) final override {
    printf("DEBUG: %s, line %d: %s\n", __FILE__, __LINE__, __PRETTY_FUNCTION__);
    return true;
  }

  unsigned int execute(function *) final override {
    printf("DEBUG: %s, line %d: %s\n", __FILE__, __LINE__, __PRETTY_FUNCTION__);
    return rtl_emit_function();
  }
};
#endif

static void llvm_start_unit(void *gcc_data, void *user_data) {
  printf("DEBUG: %s, line %d: %s\n", __FILE__, __LINE__, __func__);
}

static unsigned int rtl_emit_function() {
  printf("DEBUG: %s, line %d: %s: %s\n", __FILE__, __LINE__, __func__,
#if (GCC_MAJOR > 4)
          flag_check_pointer_bounds ? "flag_check_pointer_bounds" :
#endif
          "!flag_check_pointer_bounds");
  return 0;
}

static void print_tree_node(tree t, int indent) {
  for (int i = 1; i < indent; i++)
    printf("  ");

  enum tree_code code = TREE_CODE(t);
  if (code == RESULT_DECL || code == PARM_DECL || code == LABEL_DECL ||
      code == VAR_DECL || code == FUNCTION_DECL) {
    tree id = DECL_NAME(t);
    const char *Name = id ? IDENTIFIER_POINTER(id) : "<unnamed>";
    printf("%s : %s\n",
#if (GCC_MAJOR > 4)
            get_tree_code_name(code),
#else
            tree_code_name[(int)code],
#endif
            Name);
  } else if (code == INTEGER_CST) {
#if (GCC_MAJOR > 4)
    if (TREE_INT_CST_NUNITS(t)) {
#else
    if (TREE_INT_CST_HIGH(t)) {
#endif
      printf("%s : high=0x%X low=0x%X\n",
#if (GCC_MAJOR > 4)
             get_tree_code_name(code), TREE_INT_CST_NUNITS(t),
#else
             tree_code_name[(int)code], TREE_INT_CST_HIGH(t),
#endif
             TREE_INT_CST_LOW(t)
            );
    } else {
      printf("%s : %i\n",
#if (GCC_MAJOR > 4)
              get_tree_code_name(code),
#else
              tree_code_name[(int)code],
#endif
              TREE_INT_CST_LOW(t));
    }
    return;
  } else {
    printf("%s\n",
#if (GCC_MAJOR > 4)
            get_tree_code_name(code)
#else
            tree_code_name[(int)code]
#endif
            );
  }
}

/* Allows to see low level AST in C and C++ frontends. */
static void pre_genericize(void *gcc_data, void *user_data) {
  tree fndecl = (tree) gcc_data;

  if (TREE_CODE(fndecl) == FUNCTION_DECL) {
    tree id = DECL_NAME(fndecl);
    const char *FnName = id ? IDENTIFIER_POINTER(DECL_NAME(fndecl)) : "<unnamed>";
    printf("%s %s\n",
#if (GCC_MAJOR > 4)
            get_tree_code_name(FUNCTION_DECL),
#else
            tree_code_name[(int)FUNCTION_DECL],
#endif
            FnName);

    tree fnbody = DECL_SAVED_TREE(fndecl);
    if (TREE_CODE(fnbody) == BIND_EXPR) {
      tree t = TREE_OPERAND(fnbody, 1);
      parse_tree(t, print_tree_node, 1);
    }
  }
}

static void llvm_finish_unit(void *gcc_data, void *user_data) {
  printf("DEBUG: %s, line %d: %s\n", __FILE__, __LINE__, __func__);
}

static void llvm_finish(void *gcc_data, void *user_data) {
  printf("DEBUG: %s, line %d: %s\n", __FILE__, __LINE__, __func__);
}

/* Initialization function that GCC calls. This plugin takes an argument
   that specifies the name of the reference pass and an instance number,
   both of which determine where the plugin pass should be inserted.  */

int plugin_init(struct plugin_name_args *plugin_info,
                struct plugin_gcc_version *version) {
  struct register_pass_info pass_info;
  const char *plugin_name = plugin_info->base_name;
  int argc = plugin_info->argc;
  struct plugin_argument *argv = plugin_info->argv;

  // FIXME: why pass_rtl_emit_function's execute *not* be hooked -flto?
  //flag_lto = "";

#if 0
  register_callback(plugin_name, PLUGIN_START_UNIT, llvm_start_unit, NULL);

  pass_info.pass =
#if (GCC_MAJOR > 4)
      new pass_rtl_emit_function(g);
#else
      &pass_rtl_emit_function.pass;
#endif
  pass_info.reference_pass_name = "expand";
  pass_info.ref_pass_instance_number = 0;
  pass_info.pos_op = PASS_POS_REPLACE;
  register_callback(plugin_name, PLUGIN_PASS_MANAGER_SETUP, NULL, &pass_info);
#endif

  register_callback(plugin_name, PLUGIN_PRE_GENERICIZE, pre_genericize, NULL);

#if 0
  register_callback(plugin_name, PLUGIN_FINISH_UNIT, llvm_finish_unit, NULL);

  register_callback(plugin_name, PLUGIN_FINISH, llvm_finish, NULL);
#endif

  return 0;
}
