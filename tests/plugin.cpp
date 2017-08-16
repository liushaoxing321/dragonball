#include "auto-host.h"
#include "config.h"
#include "system.h"
#include "gcc-plugin.h"
#include "coretypes.h"
#include "tree.h"
#include "tree-pass.h"
#include "intl.h"
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
    flag_check_pointer_bounds = true;
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
          flag_check_pointer_bounds ? "flag_check_pointer_bounds"
          : "!flag_check_pointer_bounds");
  return 0;
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

  /* Other PASS replacement */
#if 0
  pass_info.pass =;
  pass_info.reference_pass_name = "PASS_NAME";
  pass_info.ref_pass_instance_number = 0;
  pass_info.pos_op = PASS_POS_REPLACE;
  register_callback(plugin_name, PLUGIN_PASS_MANAGER_SETUP, NULL, &pass_info);
#endif

  register_callback(plugin_name, PLUGIN_FINISH_UNIT, llvm_finish_unit, NULL);

  register_callback(plugin_name, PLUGIN_FINISH, llvm_finish, NULL);

  return 0;
}
