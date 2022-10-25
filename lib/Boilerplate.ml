(**
   Boilerplate to be used as a template when mapping the kotlin CST
   to another type of tree.
*)

(* Disable warnings against unused variables *)
[@@@warning "-26-27"]

(* Disable warning against unused 'rec' *)
[@@@warning "-39"]

type env = unit

let token (env : env) (_tok : Tree_sitter_run.Token.t) =
  failwith "not implemented"

let blank (env : env) () =
  failwith "not implemented"

let todo (env : env) _ =
   failwith "not implemented"

let map_comparison_operator (env : env) (x : CST.comparison_operator) =
  (match x with
  | `LT tok -> (* "<" *) token env tok
  | `GT tok -> (* ">" *) token env tok
  | `LTEQ tok -> (* "<=" *) token env tok
  | `GTEQ tok -> (* ">=" *) token env tok
  )

let map_use_site_target (env : env) ((v1, v2) : CST.use_site_target) =
  let v1 =
    (match v1 with
    | `Field tok -> (* "field" *) token env tok
    | `Prop tok -> (* "property" *) token env tok
    | `Get tok -> (* "get" *) token env tok
    | `Set tok -> (* "set" *) token env tok
    | `Rece tok -> (* "receiver" *) token env tok
    | `Param tok -> (* "param" *) token env tok
    | `Setp tok -> (* "setparam" *) token env tok
    | `Dele tok -> (* "delegate" *) token env tok
    )
  in
  let v2 = (* ":" *) token env v2 in
  todo env (v1, v2)

let map_hex_literal (env : env) (tok : CST.hex_literal) =
  (* hex_literal *) token env tok

let map_parameter_modifier (env : env) (x : CST.parameter_modifier) =
  (match x with
  | `Vararg tok -> (* "vararg" *) token env tok
  | `Noin tok -> (* "noinline" *) token env tok
  | `Cros tok -> (* "crossinline" *) token env tok
  )

let map_pat_831065d (env : env) (tok : CST.pat_831065d) =
  (* pattern \$[a-zA-Z_][a-zA-Z_0-9]* *) token env tok

let map_import_list_delimiter (env : env) (tok : CST.import_list_delimiter) =
  (* import_list_delimiter *) token env tok

let map_real_literal (env : env) (tok : CST.real_literal) =
  (* real_literal *) token env tok

let map_visibility_modifier (env : env) (x : CST.visibility_modifier) =
  (match x with
  | `Public tok -> (* "public" *) token env tok
  | `Priv tok -> (* "private" *) token env tok
  | `Inte tok -> (* "internal" *) token env tok
  | `Prot tok -> (* "protected" *) token env tok
  )

let map_pat_f630af3 (env : env) (tok : CST.pat_f630af3) =
  (* pattern [^\r\n]* *) token env tok

let map_in_operator (env : env) (x : CST.in_operator) =
  (match x with
  | `In tok -> (* "in" *) token env tok
  | `BANGin tok -> (* "!in" *) token env tok
  )

let map_automatic_semicolon (env : env) (tok : CST.automatic_semicolon) =
  (* automatic_semicolon *) token env tok

let map_multi_line_str_text (env : env) (tok : CST.multi_line_str_text) =
  (* pattern "[^\"$]+" *) token env tok

let map_pat_b294348 (env : env) (tok : CST.pat_b294348) =
  (* pattern "[^\\n\\r'\\\\]" *) token env tok

let map_additive_operator (env : env) (x : CST.additive_operator) =
  (match x with
  | `PLUS tok -> (* "+" *) token env tok
  | `DASH tok -> (* "-" *) token env tok
  )

let map_variance_modifier (env : env) (x : CST.variance_modifier) =
  (match x with
  | `In tok -> (* "in" *) token env tok
  | `Out tok -> (* "out" *) token env tok
  )

let map_class_modifier (env : env) (x : CST.class_modifier) =
  (match x with
  | `Sealed tok -> (* "sealed" *) token env tok
  | `Anno tok -> (* "annotation" *) token env tok
  | `Data tok -> (* "data" *) token env tok
  | `Inner tok -> (* "inner" *) token env tok
  )

let map_function_modifier (env : env) (x : CST.function_modifier) =
  (match x with
  | `Tail tok -> (* "tailrec" *) token env tok
  | `Op tok -> (* "operator" *) token env tok
  | `Infix tok -> (* "infix" *) token env tok
  | `Inline tok -> (* "inline" *) token env tok
  | `Exte tok -> (* "external" *) token env tok
  | `Susp tok -> (* "suspend" *) token env tok
  )

let map_equality_operator (env : env) (x : CST.equality_operator) =
  (match x with
  | `BANGEQ tok -> (* "!=" *) token env tok
  | `BANGEQEQ tok -> (* "!==" *) token env tok
  | `EQEQ tok -> (* "==" *) token env tok
  | `EQEQEQ tok -> (* "===" *) token env tok
  )

let map_label (env : env) (tok : CST.label) =
  (* label *) token env tok

let map_platform_modifier (env : env) (x : CST.platform_modifier) =
  (match x with
  | `Expect tok -> (* "expect" *) token env tok
  | `Actual tok -> (* "actual" *) token env tok
  )

let map_boolean_literal (env : env) (x : CST.boolean_literal) =
  (match x with
  | `True tok -> (* "true" *) token env tok
  | `False tok -> (* "false" *) token env tok
  )

let map_backtick_identifier (env : env) (tok : CST.backtick_identifier) =
  (* pattern `[^\r\n`]+` *) token env tok

let map_pat_c793459 (env : env) (tok : CST.pat_c793459) =
  (* pattern [uU] *) token env tok

let map_prefix_unary_operator (env : env) (x : CST.prefix_unary_operator) =
  (match x with
  | `PLUSPLUS tok -> (* "++" *) token env tok
  | `DASHDASH tok -> (* "--" *) token env tok
  | `DASH tok -> (* "-" *) token env tok
  | `PLUS tok -> (* "+" *) token env tok
  | `BANG tok -> (* "!" *) token env tok
  )

let map_as_operator (env : env) (x : CST.as_operator) =
  (match x with
  | `As tok -> (* "as" *) token env tok
  | `AsQM tok -> (* "as?" *) token env tok
  )

let map_line_str_text (env : env) (tok : CST.line_str_text) =
  (* pattern "[^\\\\\"$]+" *) token env tok

let map_postfix_unary_operator (env : env) (x : CST.postfix_unary_operator) =
  (match x with
  | `PLUSPLUS tok -> (* "++" *) token env tok
  | `DASHDASH tok -> (* "--" *) token env tok
  | `BANGBANG tok -> (* "!!" *) token env tok
  )

let map_integer_literal (env : env) (tok : CST.integer_literal) =
  (* integer_literal *) token env tok

let map_escaped_identifier (env : env) (tok : CST.escaped_identifier) =
  (* pattern "\\\\[tbrn'\"\\\\$]" *) token env tok

let map_pat_a2e2132 (env : env) (tok : CST.pat_a2e2132) =
  (* pattern [0-9a-fA-F]{4} *) token env tok

let map_safe_nav (env : env) (tok : CST.safe_nav) =
  (* safe_nav *) token env tok

let map_multiplicative_operator (env : env) (x : CST.multiplicative_operator) =
  (match x with
  | `STAR tok -> (* "*" *) token env tok
  | `SLASH tok -> (* "/" *) token env tok
  | `PERC tok -> (* "%" *) token env tok
  )

let map_anon_choice_val_2833752 (env : env) (x : CST.anon_choice_val_2833752) =
  (match x with
  | `Val tok -> (* "val" *) token env tok
  | `Var tok -> (* "var" *) token env tok
  )

let map_inheritance_modifier (env : env) (x : CST.inheritance_modifier) =
  (match x with
  | `Abst tok -> (* "abstract" *) token env tok
  | `Final tok -> (* "final" *) token env tok
  | `Open tok -> (* "open" *) token env tok
  )

let map_bin_literal (env : env) (tok : CST.bin_literal) =
  (* bin_literal *) token env tok

let map_assignment_and_operator (env : env) (x : CST.assignment_and_operator) =
  (match x with
  | `PLUSEQ tok -> (* "+=" *) token env tok
  | `DASHEQ tok -> (* "-=" *) token env tok
  | `STAREQ tok -> (* "*=" *) token env tok
  | `SLASHEQ tok -> (* "/=" *) token env tok
  | `PERCEQ tok -> (* "%=" *) token env tok
  )

let map_is_operator (env : env) (x : CST.is_operator) =
  (match x with
  | `Is tok -> (* "is" *) token env tok
  | `BANGis tok -> (* "!is" *) token env tok
  )

let map_member_modifier (env : env) (x : CST.member_modifier) =
  (match x with
  | `Over tok -> (* "override" *) token env tok
  | `Late tok -> (* "lateinit" *) token env tok
  )

let map_alpha_identifier (env : env) (tok : CST.alpha_identifier) =
  (* pattern [a-zA-Z_][a-zA-Z_0-9]* *) token env tok

let map_shebang_line (env : env) ((v1, v2) : CST.shebang_line) =
  let v1 = (* "#!" *) token env v1 in
  let v2 = map_pat_f630af3 env v2 in
  todo env (v1, v2)

let map_multi_line_string_content (env : env) (x : CST.multi_line_string_content) =
  (match x with
  | `Multi_line_str_text tok ->
      (* pattern "[^\"$]+" *) token env tok
  | `DQUOT tok -> (* "\"" *) token env tok
  )

let map_type_projection_modifier (env : env) (x : CST.type_projection_modifier) =
  map_variance_modifier env x

let map_member_access_operator (env : env) (x : CST.member_access_operator) =
  (match x with
  | `DOT tok -> (* "." *) token env tok
  | `COLONCOLON tok -> (* "::" *) token env tok
  | `Safe_nav tok -> (* safe_nav *) token env tok
  )

let map_anon_choice_int_lit_9015f32 (env : env) (x : CST.anon_choice_int_lit_9015f32) =
  (match x with
  | `Int_lit tok -> (* integer_literal *) token env tok
  | `Hex_lit tok -> (* hex_literal *) token env tok
  | `Bin_lit tok -> (* bin_literal *) token env tok
  )

let map_modifier (env : env) (x : CST.modifier) =
  (match x with
  | `Class_modi x -> map_class_modifier env x
  | `Member_modi x -> map_member_modifier env x
  | `Visi_modi x -> map_visibility_modifier env x
  | `Func_modi x -> map_function_modifier env x
  | `Prop_modi tok -> (* "const" *) token env tok
  | `Inhe_modi x -> map_inheritance_modifier env x
  | `Param_modi x -> map_parameter_modifier env x
  | `Plat_modi x -> map_platform_modifier env x
  )

let map_lexical_identifier (env : env) (x : CST.lexical_identifier) =
  (match x with
  | `Alpha_id tok ->
      (* pattern [a-zA-Z_][a-zA-Z_0-9]* *) token env tok
  | `Back_id tok -> (* pattern `[^\r\n`]+` *) token env tok
  )

let map_type_projection_modifiers (env : env) (xs : CST.type_projection_modifiers) =
  List.map (map_type_projection_modifier env) xs

let map_character_escape_seq (env : env) (x : CST.character_escape_seq) =
  (match x with
  | `Uni_char_lit (v1, v2) ->
      let v1 = (* "\\u" *) token env v1 in
      let v2 = map_pat_a2e2132 env v2 in
      todo env (v1, v2)
  | `Esca_id tok ->
      (* pattern "\\\\[tbrn'\"\\\\$]" *) token env tok
  )

let map_simple_identifier (env : env) (x : CST.simple_identifier) =
  (match x with
  | `Choice_lexi_id x ->
      (match x with
      | `Lexi_id x -> map_lexical_identifier env x
      | `Expect tok -> (* "expect" *) token env tok
      | `Data tok -> (* "data" *) token env tok
      | `Inner tok -> (* "inner" *) token env tok
      | `Actual tok -> (* "actual" *) token env tok
      | `Set tok -> (* "set" *) token env tok
      | `Get tok -> (* "get" *) token env tok
      )
  | `Pat_831065d x -> map_pat_831065d env x
  )

let map_return_at (env : env) ((v1, v2) : CST.return_at) =
  let v1 = (* "return@" *) token env v1 in
  let v2 = map_lexical_identifier env v2 in
  todo env (v1, v2)

let map_line_string_content (env : env) (x : CST.line_string_content) =
  (match x with
  | `Line_str_text tok ->
      (* pattern "[^\\\\\"$]+" *) token env tok
  | `Char_esc_seq x -> map_character_escape_seq env x
  )

let map_identifier (env : env) ((v1, v2) : CST.identifier) =
  let v1 = map_simple_identifier env v1 in
  let v2 =
    List.map (fun (v1, v2) ->
      let v1 = (* "." *) token env v1 in
      let v2 = map_simple_identifier env v2 in
      todo env (v1, v2)
    ) v2
  in
  todo env (v1, v2)

let map_import_alias (env : env) ((v1, v2) : CST.import_alias) =
  let v1 = (* "as" *) token env v1 in
  let v2 = map_simple_identifier env v2 in
  todo env (v1, v2)

let map_literal_constant (env : env) (x : CST.literal_constant) =
  (match x with
  | `Bool_lit x -> map_boolean_literal env x
  | `Int_lit tok -> (* integer_literal *) token env tok
  | `Hex_lit tok -> (* hex_literal *) token env tok
  | `Bin_lit tok -> (* bin_literal *) token env tok
  | `Char_lit (v1, v2, v3) ->
      let v1 = (* "'" *) token env v1 in
      let v2 =
        (match v2 with
        | `Char_esc_seq x -> map_character_escape_seq env x
        | `Pat_b294348 x -> map_pat_b294348 env x
        )
      in
      let v3 = (* "'" *) token env v3 in
      todo env (v1, v2, v3)
  | `Real_lit tok -> (* real_literal *) token env tok
  | `Null tok -> (* "null" *) token env tok
  | `Long_lit (v1, v2) ->
      let v1 = map_anon_choice_int_lit_9015f32 env v1 in
      let v2 = (* "L" *) token env v2 in
      todo env (v1, v2)
  | `Unsi_lit (v1, v2, v3) ->
      let v1 = map_anon_choice_int_lit_9015f32 env v1 in
      let v2 = map_pat_c793459 env v2 in
      let v3 =
        (match v3 with
        | Some tok -> (* "L" *) token env tok
        | None -> todo env ())
      in
      todo env (v1, v2, v3)
  )

let map_package_header (env : env) ((v1, v2, v3) : CST.package_header) =
  let v1 = (* "package" *) token env v1 in
  let v2 = map_identifier env v2 in
  let v3 = (* automatic_semicolon *) token env v3 in
  todo env (v1, v2, v3)

let map_import_header (env : env) ((v1, v2, v3, v4) : CST.import_header) =
  let v1 = (* "import" *) token env v1 in
  let v2 = map_identifier env v2 in
  let v3 =
    (match v3 with
    | Some x ->
        (match x with
        | `DOTSTAR v1 -> (* ".*" *) token env v1
        | `Import_alias x -> map_import_alias env x
        )
    | None -> todo env ())
  in
  let v4 = (* automatic_semicolon *) token env v4 in
  todo env (v1, v2, v3, v4)

let rec map_annotated_lambda (env : env) ((v1, v2, v3) : CST.annotated_lambda) =
  let v1 = List.map (map_annotation env) v1 in
  let v2 =
    (match v2 with
    | Some tok -> (* label *) token env tok
    | None -> todo env ())
  in
  let v3 = map_lambda_literal env v3 in
  todo env (v1, v2, v3)

and map_annotation (env : env) (x : CST.annotation) =
  (match x with
  | `Single_anno (v1, v2, v3) ->
      let v1 = (* "@" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> map_use_site_target env x
        | None -> todo env ())
      in
      let v3 = map_unescaped_annotation env v3 in
      todo env (v1, v2, v3)
  | `Multi_anno (v1, v2, v3, v4, v5) ->
      let v1 = (* "@" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> map_use_site_target env x
        | None -> todo env ())
      in
      let v3 = (* "[" *) token env v3 in
      let v4 = List.map (map_unescaped_annotation env) v4 in
      let v5 = (* "]" *) token env v5 in
      todo env (v1, v2, v3, v4, v5)
  )

and map_anon_choice_param_b77c1d8 (env : env) (x : CST.anon_choice_param_b77c1d8) =
  (match x with
  | `Param x -> map_parameter env x
  | `Type x -> map_type_ env x
  )

and map_anon_opt_rece_type_opt_DOT_cc9388e (env : env) (opt : CST.anon_opt_rece_type_opt_DOT_cc9388e) =
  (match opt with
  | Some (v1, v2) ->
      let v1 = map_receiver_type env v1 in
      let v2 =
        (match v2 with
        | Some tok -> (* "." *) token env tok
        | None -> todo env ())
      in
      todo env (v1, v2)
  | None -> todo env ())

and map_anonymous_initializer (env : env) ((v1, v2) : CST.anonymous_initializer) =
  let v1 = (* "init" *) token env v1 in
  let v2 = map_block env v2 in
  todo env (v1, v2)

and map_assignment (env : env) (x : CST.assignment) =
  (match x with
  | `Dire_assi_exp_assign_and_op_exp (v1, v2, v3) ->
      let v1 = map_directly_assignable_expression env v1 in
      let v2 = map_assignment_and_operator env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Dire_assi_exp_EQ_exp (v1, v2, v3) ->
      let v1 = map_directly_assignable_expression env v1 in
      let v2 = (* "=" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  )

and map_binary_expression (env : env) (x : CST.binary_expression) =
  (match x with
  | `Mult_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = map_multiplicative_operator env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Addi_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = map_additive_operator env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Range_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* ".." *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Infix_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = map_simple_identifier env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Elvis_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "?:" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Check_exp (v1, v2) ->
      let v1 = map_expression env v1 in
      let v2 =
        (match v2 with
        | `In_op_exp x -> map_range_test env x
        | `Is_op_type x -> map_type_test env x
        )
      in
      todo env (v1, v2)
  | `Comp_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = map_comparison_operator env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Equa_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = map_equality_operator env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Conj_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "&&" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Disj_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "||" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  )

and map_block (env : env) ((v1, v2, v3) : CST.block) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> map_statements env x
    | None -> todo env ())
  in
  let v3 = (* "}" *) token env v3 in
  todo env (v1, v2, v3)

and map_call_suffix (env : env) ((v1, v2) : CST.call_suffix) =
  let v1 =
    (match v1 with
    | Some x -> map_type_arguments env x
    | None -> todo env ())
  in
  let v2 =
    (match v2 with
    | `Opt_value_args_anno_lambda (v1, v2) ->
        let v1 =
          (match v1 with
          | Some x -> map_value_arguments env x
          | None -> todo env ())
        in
        let v2 = map_annotated_lambda env v2 in
        todo env (v1, v2)
    | `Value_args x -> map_value_arguments env x
    )
  in
  todo env (v1, v2)

and map_catch_block (env : env) ((v1, v2, v3, v4, v5, v6, v7, v8) : CST.catch_block) =
  let v1 = (* "catch" *) token env v1 in
  let v2 = (* "(" *) token env v2 in
  let v3 = List.map (map_annotation env) v3 in
  let v4 = map_simple_identifier env v4 in
  let v5 = (* ":" *) token env v5 in
  let v6 = map_type_ env v6 in
  let v7 = (* ")" *) token env v7 in
  let v8 = map_block env v8 in
  todo env (v1, v2, v3, v4, v5, v6, v7, v8)

and map_class_body (env : env) ((v1, v2, v3) : CST.class_body) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> map_class_member_declarations env x
    | None -> todo env ())
  in
  let v3 = (* "}" *) token env v3 in
  todo env (v1, v2, v3)

and map_class_declaration (env : env) (x : CST.class_declaration) =
  (match x with
  | `Opt_modifs_choice_class_simple_id_opt_type_params_opt_prim_cons_opt_COLON_dele_specis_opt_type_consts_opt_class_body (v1, v2, v3, v4, v5, v6, v7, v8) ->
      let v1 =
        (match v1 with
        | Some x -> map_modifiers env x
        | None -> todo env ())
      in
      let v2 =
        (match v2 with
        | `Class tok -> (* "class" *) token env tok
        | `Inte tok -> (* "interface" *) token env tok
        )
      in
      let v3 = map_simple_identifier env v3 in
      let v4 =
        (match v4 with
        | Some x -> map_type_parameters env x
        | None -> todo env ())
      in
      let v5 =
        (match v5 with
        | Some x -> map_primary_constructor env x
        | None -> todo env ())
      in
      let v6 =
        (match v6 with
        | Some (v1, v2) ->
            let v1 = (* ":" *) token env v1 in
            let v2 = map_delegation_specifiers env v2 in
            todo env (v1, v2)
        | None -> todo env ())
      in
      let v7 =
        (match v7 with
        | Some x -> map_type_constraints env x
        | None -> todo env ())
      in
      let v8 =
        (match v8 with
        | Some x -> map_class_body env x
        | None -> todo env ())
      in
      todo env (v1, v2, v3, v4, v5, v6, v7, v8)
  | `Opt_modifs_enum_class_simple_id_opt_type_params_opt_prim_cons_opt_COLON_dele_specis_opt_type_consts_opt_enum_class_body (v1, v2, v3, v4, v5, v6, v7, v8, v9) ->
      let v1 =
        (match v1 with
        | Some x -> map_modifiers env x
        | None -> todo env ())
      in
      let v2 = (* "enum" *) token env v2 in
      let v3 = (* "class" *) token env v3 in
      let v4 = map_simple_identifier env v4 in
      let v5 =
        (match v5 with
        | Some x -> map_type_parameters env x
        | None -> todo env ())
      in
      let v6 =
        (match v6 with
        | Some x -> map_primary_constructor env x
        | None -> todo env ())
      in
      let v7 =
        (match v7 with
        | Some (v1, v2) ->
            let v1 = (* ":" *) token env v1 in
            let v2 = map_delegation_specifiers env v2 in
            todo env (v1, v2)
        | None -> todo env ())
      in
      let v8 =
        (match v8 with
        | Some x -> map_type_constraints env x
        | None -> todo env ())
      in
      let v9 =
        (match v9 with
        | Some x -> map_enum_class_body env x
        | None -> todo env ())
      in
      todo env (v1, v2, v3, v4, v5, v6, v7, v8, v9)
  )

and map_class_member_declaration (env : env) (x : CST.class_member_declaration) =
  (match x with
  | `Choice_decl x ->
      (match x with
      | `Decl x -> map_declaration env x
      | `Comp_obj x -> map_companion_object env x
      | `Anon_init x -> map_anonymous_initializer env x
      | `Seco_cons x -> map_secondary_constructor env x
      )
  | `Ellips tok -> (* "..." *) token env tok
  )

and map_class_member_declarations (env : env) (xs : CST.class_member_declarations) =
  List.map (fun (v1, v2) ->
    let v1 = map_class_member_declaration env v1 in
    let v2 = (* automatic_semicolon *) token env v2 in
    todo env (v1, v2)
  ) xs

and map_class_parameter (env : env) (x : CST.class_parameter) =
  (match x with
  | `Opt_modifs_opt_choice_val_simple_id_COLON_type_opt_EQ_exp (v1, v2, v3, v4, v5, v6) ->
      let v1 =
        (match v1 with
        | Some x -> map_modifiers env x
        | None -> todo env ())
      in
      let v2 =
        (match v2 with
        | Some x -> map_anon_choice_val_2833752 env x
        | None -> todo env ())
      in
      let v3 = map_simple_identifier env v3 in
      let v4 = (* ":" *) token env v4 in
      let v5 = map_type_ env v5 in
      let v6 =
        (match v6 with
        | Some (v1, v2) ->
            let v1 = (* "=" *) token env v1 in
            let v2 = map_expression env v2 in
            todo env (v1, v2)
        | None -> todo env ())
      in
      todo env (v1, v2, v3, v4, v5, v6)
  | `Ellips tok -> (* "..." *) token env tok
  )

and map_class_parameters (env : env) ((v1, v2, v3, v4) : CST.class_parameters) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) ->
        let v1 = map_class_parameter env v1 in
        let v2 =
          List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_class_parameter env v2 in
            todo env (v1, v2)
          ) v2
        in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v3 =
    (match v3 with
    | Some tok -> (* "," *) token env tok
    | None -> todo env ())
  in
  let v4 = (* ")" *) token env v4 in
  todo env (v1, v2, v3, v4)

and map_companion_object (env : env) ((v1, v2, v3, v4, v5, v6) : CST.companion_object) =
  let v1 =
    (match v1 with
    | Some x -> map_modifiers env x
    | None -> todo env ())
  in
  let v2 = (* "companion" *) token env v2 in
  let v3 = (* "object" *) token env v3 in
  let v4 =
    (match v4 with
    | Some x -> map_simple_identifier env x
    | None -> todo env ())
  in
  let v5 =
    (match v5 with
    | Some (v1, v2) ->
        let v1 = (* ":" *) token env v1 in
        let v2 = map_delegation_specifiers env v2 in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v6 =
    (match v6 with
    | Some x -> map_class_body env x
    | None -> todo env ())
  in
  todo env (v1, v2, v3, v4, v5, v6)

and map_constructor_delegation_call (env : env) ((v1, v2) : CST.constructor_delegation_call) =
  let v1 =
    (match v1 with
    | `This tok -> (* "this" *) token env tok
    | `Super tok -> (* "super" *) token env tok
    )
  in
  let v2 = map_value_arguments env v2 in
  todo env (v1, v2)

and map_constructor_invocation (env : env) ((v1, v2) : CST.constructor_invocation) =
  let v1 = map_user_type env v1 in
  let v2 = map_value_arguments env v2 in
  todo env (v1, v2)

and map_control_structure_body (env : env) (x : CST.control_structure_body) =
  (match x with
  | `Blk x -> map_block env x
  | `Stmt x -> map_statement env x
  )

and map_declaration (env : env) (x : CST.declaration) =
  (match x with
  | `Class_decl x -> map_class_declaration env x
  | `Obj_decl (v1, v2, v3, v4, v5) ->
      let v1 =
        (match v1 with
        | Some x -> map_modifiers env x
        | None -> todo env ())
      in
      let v2 = (* "object" *) token env v2 in
      let v3 = map_simple_identifier env v3 in
      let v4 =
        (match v4 with
        | Some (v1, v2) ->
            let v1 = (* ":" *) token env v1 in
            let v2 = map_delegation_specifiers env v2 in
            todo env (v1, v2)
        | None -> todo env ())
      in
      let v5 =
        (match v5 with
        | Some x -> map_class_body env x
        | None -> todo env ())
      in
      todo env (v1, v2, v3, v4, v5)
  | `Func_decl (v1, v2, v3, v4, v5, v6, v7, v8, v9) ->
      let v1 =
        (match v1 with
        | Some x -> map_modifiers env x
        | None -> todo env ())
      in
      let v2 = (* "fun" *) token env v2 in
      let v3 =
        (match v3 with
        | Some x -> map_type_parameters env x
        | None -> todo env ())
      in
      let v4 = map_anon_opt_rece_type_opt_DOT_cc9388e env v4 in
      let v5 = map_simple_identifier env v5 in
      let v6 = map_function_value_parameters env v6 in
      let v7 =
        (match v7 with
        | Some (v1, v2) ->
            let v1 = (* ":" *) token env v1 in
            let v2 = map_type_ env v2 in
            todo env (v1, v2)
        | None -> todo env ())
      in
      let v8 =
        (match v8 with
        | Some x -> map_type_constraints env x
        | None -> todo env ())
      in
      let v9 =
        (match v9 with
        | Some x -> map_function_body env x
        | None -> todo env ())
      in
      todo env (v1, v2, v3, v4, v5, v6, v7, v8, v9)
  | `Prop_decl (v1, v2, v3, v4, v5, v6, v7, v8, v9) ->
      let v1 =
        (match v1 with
        | Some x -> map_modifiers env x
        | None -> todo env ())
      in
      let v2 = map_anon_choice_val_2833752 env v2 in
      let v3 =
        (match v3 with
        | Some x -> map_type_parameters env x
        | None -> todo env ())
      in
      let v4 = map_anon_opt_rece_type_opt_DOT_cc9388e env v4 in
      let v5 = map_lambda_parameter env v5 in
      let v6 =
        (match v6 with
        | Some x -> map_type_constraints env x
        | None -> todo env ())
      in
      let v7 =
        (match v7 with
        | Some x ->
            (match x with
            | `EQ_exp (v1, v2) ->
                let v1 = (* "=" *) token env v1 in
                let v2 = map_expression env v2 in
                todo env (v1, v2)
            | `Prop_dele x -> map_property_delegate env x
            )
        | None -> todo env ())
      in
      let v8 =
        (match v8 with
        | Some tok -> (* ";" *) token env tok
        | None -> todo env ())
      in
      let v9 =
        (match v9 with
        | `Opt_getter opt ->
            (match opt with
            | Some x -> map_getter env x
            | None -> todo env ())
        | `Opt_setter opt ->
            (match opt with
            | Some x -> map_setter env x
            | None -> todo env ())
        )
      in
      todo env (v1, v2, v3, v4, v5, v6, v7, v8, v9)
  | `Getter x -> map_getter env x
  | `Setter x -> map_setter env x
  | `Type_alias (v1, v2, v3, v4, v5) ->
      let v1 =
        (match v1 with
        | Some x -> map_modifiers env x
        | None -> todo env ())
      in
      let v2 = (* "typealias" *) token env v2 in
      let v3 = map_simple_identifier env v3 in
      let v4 = (* "=" *) token env v4 in
      let v5 = map_type_ env v5 in
      todo env (v1, v2, v3, v4, v5)
  )

and map_delegation_specifier (env : env) (x : CST.delegation_specifier) =
  (match x with
  | `Cons_invo x -> map_constructor_invocation env x
  | `Expl_dele (v1, v2, v3) ->
      let v1 =
        (match v1 with
        | `User_type x -> map_user_type env x
        | `Func_type x -> map_function_type env x
        )
      in
      let v2 = (* "by" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `User_type x -> map_user_type env x
  | `Func_type x -> map_function_type env x
  )

and map_delegation_specifiers (env : env) ((v1, v2) : CST.delegation_specifiers) =
  let v1 = map_delegation_specifier env v1 in
  let v2 =
    List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_delegation_specifier env v2 in
      todo env (v1, v2)
    ) v2
  in
  todo env (v1, v2)

and map_directly_assignable_expression (env : env) (x : CST.directly_assignable_expression) =
  (match x with
  | `Post_un_exp (v1, v2) ->
      let v1 = map_primary_expression env v1 in
      let v2 = List.map (map_postfix_unary_suffix env) v2 in
      todo env (v1, v2)
  | `Simple_id x -> map_simple_identifier env x
  )

and map_enum_class_body (env : env) ((v1, v2, v3, v4) : CST.enum_class_body) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> map_enum_entries env x
    | None -> todo env ())
  in
  let v3 =
    (match v3 with
    | Some (v1, v2) ->
        let v1 = (* ";" *) token env v1 in
        let v2 =
          (match v2 with
          | Some x -> map_class_member_declarations env x
          | None -> todo env ())
        in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v4 = (* "}" *) token env v4 in
  todo env (v1, v2, v3, v4)

and map_enum_entries (env : env) ((v1, v2, v3) : CST.enum_entries) =
  let v1 = map_enum_entry env v1 in
  let v2 =
    List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_enum_entry env v2 in
      todo env (v1, v2)
    ) v2
  in
  let v3 =
    (match v3 with
    | Some tok -> (* "," *) token env tok
    | None -> todo env ())
  in
  todo env (v1, v2, v3)

and map_enum_entry (env : env) ((v1, v2, v3, v4) : CST.enum_entry) =
  let v1 =
    (match v1 with
    | Some x -> map_modifiers env x
    | None -> todo env ())
  in
  let v2 = map_simple_identifier env v2 in
  let v3 =
    (match v3 with
    | Some x -> map_value_arguments env x
    | None -> todo env ())
  in
  let v4 =
    (match v4 with
    | Some x -> map_class_body env x
    | None -> todo env ())
  in
  todo env (v1, v2, v3, v4)

and map_expression (env : env) (x : CST.expression) =
  (match x with
  | `Choice_un_exp x ->
      (match x with
      | `Un_exp x -> map_unary_expression env x
      | `Bin_exp x -> map_binary_expression env x
      | `Prim_exp x -> map_primary_expression env x
      )
  | `Ellips tok -> (* "..." *) token env tok
  | `Deep_ellips (v1, v2, v3) ->
      let v1 = (* "<..." *) token env v1 in
      let v2 = map_expression env v2 in
      let v3 = (* "...>" *) token env v3 in
      todo env (v1, v2, v3)
  )

and map_finally_block (env : env) ((v1, v2) : CST.finally_block) =
  let v1 = (* "finally" *) token env v1 in
  let v2 = map_block env v2 in
  todo env (v1, v2)

and map_function_body (env : env) (x : CST.function_body) =
  (match x with
  | `Blk x -> map_block env x
  | `EQ_exp (v1, v2) ->
      let v1 = (* "=" *) token env v1 in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
  )

and map_function_literal (env : env) (x : CST.function_literal) =
  (match x with
  | `Lambda_lit x -> map_lambda_literal env x
  | `Anon_func (v1, v2, v3, v4, v5) ->
      let v1 = (* "fun" *) token env v1 in
      let v2 =
        (match v2 with
        | Some (v1, v2, v3) ->
            let v1 = map_simple_user_type env v1 in
            let v2 =
              List.map (fun (v1, v2) ->
                let v1 = (* "." *) token env v1 in
                let v2 = map_simple_user_type env v2 in
                todo env (v1, v2)
              ) v2
            in
            let v3 = (* "." *) token env v3 in
            todo env (v1, v2, v3)
        | None -> todo env ())
      in
      let v3 = map_function_value_parameters env v3 in
      let v4 =
        (match v4 with
        | Some (v1, v2) ->
            let v1 = (* ":" *) token env v1 in
            let v2 = map_type_ env v2 in
            todo env (v1, v2)
        | None -> todo env ())
      in
      let v5 =
        (match v5 with
        | Some x -> map_function_body env x
        | None -> todo env ())
      in
      todo env (v1, v2, v3, v4, v5)
  )

and map_function_type (env : env) ((v1, v2, v3, v4) : CST.function_type) =
  let v1 =
    (match v1 with
    | Some (v1, v2) ->
        let v1 = map_simple_user_type env v1 in
        let v2 = (* "." *) token env v2 in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v2 = map_function_type_parameters env v2 in
  let v3 = (* "->" *) token env v3 in
  let v4 = map_type_ env v4 in
  todo env (v1, v2, v3, v4)

and map_function_type_parameters (env : env) ((v1, v2, v3) : CST.function_type_parameters) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) ->
        let v1 = map_anon_choice_param_b77c1d8 env v1 in
        let v2 =
          List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_anon_choice_param_b77c1d8 env v2 in
            todo env (v1, v2)
          ) v2
        in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v3 = (* ")" *) token env v3 in
  todo env (v1, v2, v3)

and map_function_value_parameter (env : env) (x : CST.function_value_parameter) =
  (match x with
  | `Opt_param_modifs_param_opt_EQ_exp (v1, v2, v3) ->
      let v1 =
        (match v1 with
        | Some x -> map_parameter_modifiers env x
        | None -> todo env ())
      in
      let v2 = map_parameter env v2 in
      let v3 =
        (match v3 with
        | Some (v1, v2) ->
            let v1 = (* "=" *) token env v1 in
            let v2 = map_expression env v2 in
            todo env (v1, v2)
        | None -> todo env ())
      in
      todo env (v1, v2, v3)
  | `Ellips tok -> (* "..." *) token env tok
  )

and map_function_value_parameters (env : env) ((v1, v2, v3, v4) : CST.function_value_parameters) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) ->
        let v1 = map_function_value_parameter env v1 in
        let v2 =
          List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_function_value_parameter env v2 in
            todo env (v1, v2)
          ) v2
        in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v3 =
    (match v3 with
    | Some tok -> (* "," *) token env tok
    | None -> todo env ())
  in
  let v4 = (* ")" *) token env v4 in
  todo env (v1, v2, v3, v4)

and map_getter (env : env) ((v1, v2, v3) : CST.getter) =
  let v1 =
    (match v1 with
    | Some x -> map_modifiers env x
    | None -> todo env ())
  in
  let v2 = (* "get" *) token env v2 in
  let v3 =
    (match v3 with
    | Some (v1, v2, v3, v4) ->
        let v1 = (* "(" *) token env v1 in
        let v2 = (* ")" *) token env v2 in
        let v3 =
          (match v3 with
          | Some (v1, v2) ->
              let v1 = (* ":" *) token env v1 in
              let v2 = map_type_ env v2 in
              todo env (v1, v2)
          | None -> todo env ())
        in
        let v4 = map_function_body env v4 in
        todo env (v1, v2, v3, v4)
    | None -> todo env ())
  in
  todo env (v1, v2, v3)

and map_indexing_suffix (env : env) ((v1, v2, v3, v4) : CST.indexing_suffix) =
  let v1 = (* "[" *) token env v1 in
  let v2 = map_expression env v2 in
  let v3 =
    List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
    ) v3
  in
  let v4 = (* "]" *) token env v4 in
  todo env (v1, v2, v3, v4)

and map_interpolation (env : env) (x : CST.interpolation) =
  (match x with
  | `DOLLARLCURL_exp_RCURL (v1, v2, v3) ->
      let v1 = (* "${" *) token env v1 in
      let v2 = map_expression env v2 in
      let v3 = (* "}" *) token env v3 in
      todo env (v1, v2, v3)
  | `DOLLAR_simple_id (v1, v2) ->
      let v1 = (* "$" *) token env v1 in
      let v2 = map_simple_identifier env v2 in
      todo env (v1, v2)
  )

and map_jump_expression (env : env) (x : CST.jump_expression) =
  (match x with
  | `Throw_exp (v1, v2) ->
      let v1 = (* "throw" *) token env v1 in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
  | `Choice_ret_opt_exp (v1, v2) ->
      let v1 =
        (match v1 with
        | `Ret tok -> (* "return" *) token env tok
        | `Ret_at x -> map_return_at env x
        )
      in
      let v2 =
        (match v2 with
        | Some x -> map_expression env x
        | None -> todo env ())
      in
      todo env (v1, v2)
  | `Cont tok -> (* "continue" *) token env tok
  | `Cont_at (v1, v2) ->
      let v1 = (* "continue@" *) token env v1 in
      let v2 = map_lexical_identifier env v2 in
      todo env (v1, v2)
  | `Brk tok -> (* "break" *) token env tok
  | `Brk_at (v1, v2) ->
      let v1 = (* "break@" *) token env v1 in
      let v2 = map_lexical_identifier env v2 in
      todo env (v1, v2)
  )

and map_lambda_literal (env : env) ((v1, v2, v3, v4) : CST.lambda_literal) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) ->
        let v1 =
          (match v1 with
          | Some x -> map_lambda_parameters env x
          | None -> todo env ())
        in
        let v2 = (* "->" *) token env v2 in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v3 =
    (match v3 with
    | Some x -> map_statements env x
    | None -> todo env ())
  in
  let v4 = (* "}" *) token env v4 in
  todo env (v1, v2, v3, v4)

and map_lambda_parameter (env : env) (x : CST.lambda_parameter) =
  (match x with
  | `Var_decl x -> map_variable_declaration env x
  | `Multi_var_decl (v1, v2, v3, v4) ->
      let v1 = (* "(" *) token env v1 in
      let v2 = map_variable_declaration env v2 in
      let v3 =
        List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_variable_declaration env v2 in
          todo env (v1, v2)
        ) v3
      in
      let v4 = (* ")" *) token env v4 in
      todo env (v1, v2, v3, v4)
  )

and map_lambda_parameters (env : env) ((v1, v2) : CST.lambda_parameters) =
  let v1 = map_lambda_parameter env v1 in
  let v2 =
    List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_lambda_parameter env v2 in
      todo env (v1, v2)
    ) v2
  in
  todo env (v1, v2)

and map_loop_statement (env : env) (x : CST.loop_statement) =
  (match x with
  | `For_stmt (v1, v2, v3, v4, v5, v6, v7, v8) ->
      let v1 = (* "for" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 = List.map (map_annotation env) v3 in
      let v4 = map_lambda_parameter env v4 in
      let v5 = (* "in" *) token env v5 in
      let v6 = map_expression env v6 in
      let v7 = (* ")" *) token env v7 in
      let v8 =
        (match v8 with
        | Some x -> map_control_structure_body env x
        | None -> todo env ())
      in
      todo env (v1, v2, v3, v4, v5, v6, v7, v8)
  | `While_stmt (v1, v2, v3, v4, v5) ->
      let v1 = (* "while" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 = map_expression env v3 in
      let v4 = (* ")" *) token env v4 in
      let v5 =
        (match v5 with
        | `SEMI tok -> (* ";" *) token env tok
        | `Cont_stru_body x -> map_control_structure_body env x
        )
      in
      todo env (v1, v2, v3, v4, v5)
  | `Do_while_stmt (v1, v2, v3, v4, v5, v6) ->
      let v1 = (* "do" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> map_control_structure_body env x
        | None -> todo env ())
      in
      let v3 = (* "while" *) token env v3 in
      let v4 = (* "(" *) token env v4 in
      let v5 = map_expression env v5 in
      let v6 = (* ")" *) token env v6 in
      todo env (v1, v2, v3, v4, v5, v6)
  )

and map_modifiers (env : env) (xs : CST.modifiers) =
  List.map (fun x ->
    (match x with
    | `Anno x -> map_annotation env x
    | `Modi x -> map_modifier env x
    )
  ) xs

and map_navigation_suffix (env : env) (x : CST.navigation_suffix) =
  (match x with
  | `Member_access_op_choice_simple_id (v1, v2) ->
      let v1 = map_member_access_operator env v1 in
      let v2 =
        (match v2 with
        | `Simple_id x -> map_simple_identifier env x
        | `Paren_exp x -> map_parenthesized_expression env x
        | `Class tok -> (* "class" *) token env tok
        )
      in
      todo env (v1, v2)
  | `Member_access_op_ellips (v1, v2) ->
      let v1 = map_member_access_operator env v1 in
      let v2 = (* "..." *) token env v2 in
      todo env (v1, v2)
  )

and map_nullable_type (env : env) ((v1, v2) : CST.nullable_type) =
  let v1 =
    (match v1 with
    | `Type_ref x -> map_type_reference env x
    | `Paren_type x -> map_parenthesized_type env x
    )
  in
  let v2 = List.map (token env (* "?" *)) v2 in
  todo env (v1, v2)

and map_parameter (env : env) ((v1, v2, v3) : CST.parameter) =
  let v1 = map_simple_identifier env v1 in
  let v2 = (* ":" *) token env v2 in
  let v3 = map_type_ env v3 in
  todo env (v1, v2, v3)

and map_parameter_modifiers (env : env) (xs : CST.parameter_modifiers) =
  List.map (fun x ->
    (match x with
    | `Anno x -> map_annotation env x
    | `Param_modi x -> map_parameter_modifier env x
    )
  ) xs

and map_parameter_with_optional_type (env : env) ((v1, v2, v3) : CST.parameter_with_optional_type) =
  let v1 =
    (match v1 with
    | Some x -> map_parameter_modifiers env x
    | None -> todo env ())
  in
  let v2 = map_simple_identifier env v2 in
  let v3 =
    (match v3 with
    | Some (v1, v2) ->
        let v1 = (* ":" *) token env v1 in
        let v2 = map_type_ env v2 in
        todo env (v1, v2)
    | None -> todo env ())
  in
  todo env (v1, v2, v3)

and map_parenthesized_expression (env : env) ((v1, v2, v3) : CST.parenthesized_expression) =
  let v1 = (* "(" *) token env v1 in
  let v2 = map_expression env v2 in
  let v3 = (* ")" *) token env v3 in
  todo env (v1, v2, v3)

and map_parenthesized_type (env : env) ((v1, v2, v3) : CST.parenthesized_type) =
  let v1 = (* "(" *) token env v1 in
  let v2 = map_type_ env v2 in
  let v3 = (* ")" *) token env v3 in
  todo env (v1, v2, v3)

and map_postfix_unary_suffix (env : env) (x : CST.postfix_unary_suffix) =
  (match x with
  | `Post_un_op x -> map_postfix_unary_operator env x
  | `Navi_suffix x -> map_navigation_suffix env x
  | `Inde_suffix x -> map_indexing_suffix env x
  )

and map_primary_constructor (env : env) ((v1, v2) : CST.primary_constructor) =
  let v1 =
    (match v1 with
    | Some (v1, v2) ->
        let v1 =
          (match v1 with
          | Some x -> map_modifiers env x
          | None -> todo env ())
        in
        let v2 = (* "constructor" *) token env v2 in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v2 = map_class_parameters env v2 in
  todo env (v1, v2)

and map_primary_expression (env : env) (x : CST.primary_expression) =
  (match x with
  | `Paren_exp x -> map_parenthesized_expression env x
  | `Simple_id x -> map_simple_identifier env x
  | `Lit_cst x -> map_literal_constant env x
  | `Str_lit x -> map_string_literal env x
  | `Call_ref (v1, v2, v3) ->
      let v1 =
        (match v1 with
        | Some x -> map_simple_identifier env x
        | None -> todo env ())
      in
      let v2 = (* "::" *) token env v2 in
      let v3 =
        (match v3 with
        | `Simple_id x -> map_simple_identifier env x
        | `Class tok -> (* "class" *) token env tok
        )
      in
      todo env (v1, v2, v3)
  | `Func_lit x -> map_function_literal env x
  | `Obj_lit (v1, v2, v3) ->
      let v1 = (* "object" *) token env v1 in
      let v2 =
        (match v2 with
        | Some (v1, v2) ->
            let v1 = (* ":" *) token env v1 in
            let v2 = map_delegation_specifiers env v2 in
            todo env (v1, v2)
        | None -> todo env ())
      in
      let v3 = map_class_body env v3 in
      todo env (v1, v2, v3)
  | `Coll_lit (v1, v2, v3, v4) ->
      let v1 = (* "[" *) token env v1 in
      let v2 = map_expression env v2 in
      let v3 =
        List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_expression env v2 in
          todo env (v1, v2)
        ) v3
      in
      let v4 = (* "]" *) token env v4 in
      todo env (v1, v2, v3, v4)
  | `This_exp tok -> (* "this" *) token env tok
  | `Super_exp v1 -> (* "super" *) token env v1
  | `If_exp (v1, v2, v3, v4, v5) ->
      let v1 = (* "if" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 = map_expression env v3 in
      let v4 = (* ")" *) token env v4 in
      let v5 =
        (match v5 with
        | `Cont_stru_body x -> map_control_structure_body env x
        | `SEMI tok -> (* ";" *) token env tok
        | `Opt_cont_stru_body_opt_SEMI_else_choice_cont_stru_body (v1, v2, v3, v4) ->
            let v1 =
              (match v1 with
              | Some x -> map_control_structure_body env x
              | None -> todo env ())
            in
            let v2 =
              (match v2 with
              | Some tok -> (* ";" *) token env tok
              | None -> todo env ())
            in
            let v3 = (* "else" *) token env v3 in
            let v4 =
              (match v4 with
              | `Cont_stru_body x -> map_control_structure_body env x
              | `SEMI tok -> (* ";" *) token env tok
              )
            in
            todo env (v1, v2, v3, v4)
        )
      in
      todo env (v1, v2, v3, v4, v5)
  | `When_exp (v1, v2, v3, v4, v5) ->
      let v1 = (* "when" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> map_when_subject env x
        | None -> todo env ())
      in
      let v3 = (* "{" *) token env v3 in
      let v4 = List.map (map_when_entry env) v4 in
      let v5 = (* "}" *) token env v5 in
      todo env (v1, v2, v3, v4, v5)
  | `Try_exp (v1, v2, v3) ->
      let v1 = (* "try" *) token env v1 in
      let v2 = map_block env v2 in
      let v3 =
        (match v3 with
        | `Rep1_catch_blk_opt_fina_blk (v1, v2) ->
            let v1 = List.map (map_catch_block env) v1 in
            let v2 =
              (match v2 with
              | Some x -> map_finally_block env x
              | None -> todo env ())
            in
            todo env (v1, v2)
        | `Fina_blk x -> map_finally_block env x
        )
      in
      todo env (v1, v2, v3)
  | `Jump_exp x -> map_jump_expression env x
  )

and map_property_delegate (env : env) ((v1, v2) : CST.property_delegate) =
  let v1 = (* "by" *) token env v1 in
  let v2 = map_expression env v2 in
  todo env (v1, v2)

and map_range_test (env : env) ((v1, v2) : CST.range_test) =
  let v1 = map_in_operator env v1 in
  let v2 = map_expression env v2 in
  todo env (v1, v2)

and map_receiver_type (env : env) ((v1, v2) : CST.receiver_type) =
  let v1 =
    (match v1 with
    | Some x -> map_type_modifiers env x
    | None -> todo env ())
  in
  let v2 =
    (match v2 with
    | `Type_ref x -> map_type_reference env x
    | `Paren_type x -> map_parenthesized_type env x
    | `Null_type x -> map_nullable_type env x
    )
  in
  todo env (v1, v2)

and map_secondary_constructor (env : env) ((v1, v2, v3, v4, v5) : CST.secondary_constructor) =
  let v1 =
    (match v1 with
    | Some x -> map_modifiers env x
    | None -> todo env ())
  in
  let v2 = (* "constructor" *) token env v2 in
  let v3 = map_function_value_parameters env v3 in
  let v4 =
    (match v4 with
    | Some (v1, v2) ->
        let v1 = (* ":" *) token env v1 in
        let v2 = map_constructor_delegation_call env v2 in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v5 =
    (match v5 with
    | Some x -> map_block env x
    | None -> todo env ())
  in
  todo env (v1, v2, v3, v4, v5)

and map_setter (env : env) ((v1, v2, v3) : CST.setter) =
  let v1 =
    (match v1 with
    | Some x -> map_modifiers env x
    | None -> todo env ())
  in
  let v2 = (* "set" *) token env v2 in
  let v3 =
    (match v3 with
    | Some (v1, v2, v3, v4, v5) ->
        let v1 = (* "(" *) token env v1 in
        let v2 = map_parameter_with_optional_type env v2 in
        let v3 = (* ")" *) token env v3 in
        let v4 =
          (match v4 with
          | Some (v1, v2) ->
              let v1 = (* ":" *) token env v1 in
              let v2 = map_type_ env v2 in
              todo env (v1, v2)
          | None -> todo env ())
        in
        let v5 = map_function_body env v5 in
        todo env (v1, v2, v3, v4, v5)
    | None -> todo env ())
  in
  todo env (v1, v2, v3)

and map_simple_user_type (env : env) ((v1, v2) : CST.simple_user_type) =
  let v1 = map_simple_identifier env v1 in
  let v2 =
    (match v2 with
    | Some x -> map_type_arguments env x
    | None -> todo env ())
  in
  todo env (v1, v2)

and map_statement (env : env) (x : CST.statement) =
  (match x with
  | `Decl x -> map_declaration env x
  | `Rep_choice_label_choice_assign (v1, v2) ->
      let v1 =
        List.map (fun x ->
          (match x with
          | `Label tok -> (* label *) token env tok
          | `Anno x -> map_annotation env x
          )
        ) v1
      in
      let v2 =
        (match v2 with
        | `Assign x -> map_assignment env x
        | `Loop_stmt x -> map_loop_statement env x
        | `Exp x -> map_expression env x
        )
      in
      todo env (v1, v2)
  )

and map_statements (env : env) ((v1, v2, v3) : CST.statements) =
  let v1 = map_statement env v1 in
  let v2 =
    List.map (fun (v1, v2) ->
      let v1 = (* automatic_semicolon *) token env v1 in
      let v2 = map_statement env v2 in
      todo env (v1, v2)
    ) v2
  in
  let v3 =
    (match v3 with
    | Some tok -> (* automatic_semicolon *) token env tok
    | None -> todo env ())
  in
  todo env (v1, v2, v3)

and map_string_literal (env : env) (x : CST.string_literal) =
  (match x with
  | `Line_str_lit (v1, v2, v3) ->
      let v1 = (* "\"" *) token env v1 in
      let v2 =
        List.map (fun x ->
          (match x with
          | `Line_str_content x -> map_line_string_content env x
          | `Interp x -> map_interpolation env x
          )
        ) v2
      in
      let v3 = (* "\"" *) token env v3 in
      todo env (v1, v2, v3)
  | `Multi_line_str_lit (v1, v2, v3) ->
      let v1 = (* "\"\"\"" *) token env v1 in
      let v2 =
        List.map (fun x ->
          (match x with
          | `Multi_line_str_content x ->
              map_multi_line_string_content env x
          | `Interp x -> map_interpolation env x
          )
        ) v2
      in
      let v3 = (* "\"\"\"" *) token env v3 in
      todo env (v1, v2, v3)
  )

and map_type_ (env : env) ((v1, v2) : CST.type_) =
  let v1 =
    (match v1 with
    | Some x -> map_type_modifiers env x
    | None -> todo env ())
  in
  let v2 =
    (match v2 with
    | `Paren_type x -> map_parenthesized_type env x
    | `Null_type x -> map_nullable_type env x
    | `Type_ref x -> map_type_reference env x
    | `Func_type x -> map_function_type env x
    )
  in
  todo env (v1, v2)

and map_type_arguments (env : env) ((v1, v2, v3, v4) : CST.type_arguments) =
  let v1 = (* "<" *) token env v1 in
  let v2 = map_type_projection env v2 in
  let v3 =
    List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_type_projection env v2 in
      todo env (v1, v2)
    ) v3
  in
  let v4 = (* ">" *) token env v4 in
  todo env (v1, v2, v3, v4)

and map_type_constraint (env : env) ((v1, v2, v3, v4) : CST.type_constraint) =
  let v1 = List.map (map_annotation env) v1 in
  let v2 = map_simple_identifier env v2 in
  let v3 = (* ":" *) token env v3 in
  let v4 = map_type_ env v4 in
  todo env (v1, v2, v3, v4)

and map_type_constraints (env : env) ((v1, v2, v3) : CST.type_constraints) =
  let v1 = (* "where" *) token env v1 in
  let v2 = map_type_constraint env v2 in
  let v3 =
    List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_type_constraint env v2 in
      todo env (v1, v2)
    ) v3
  in
  todo env (v1, v2, v3)

and map_type_modifier (env : env) (x : CST.type_modifier) =
  (match x with
  | `Anno x -> map_annotation env x
  | `Susp tok -> (* "suspend" *) token env tok
  )

and map_type_modifiers (env : env) (xs : CST.type_modifiers) =
  List.map (map_type_modifier env) xs

and map_type_parameter (env : env) ((v1, v2, v3) : CST.type_parameter) =
  let v1 =
    (match v1 with
    | Some x -> map_type_parameter_modifiers env x
    | None -> todo env ())
  in
  let v2 = map_simple_identifier env v2 in
  let v3 =
    (match v3 with
    | Some (v1, v2) ->
        let v1 = (* ":" *) token env v1 in
        let v2 = map_type_ env v2 in
        todo env (v1, v2)
    | None -> todo env ())
  in
  todo env (v1, v2, v3)

and map_type_parameter_modifier (env : env) (x : CST.type_parameter_modifier) =
  (match x with
  | `Reif_modi tok -> (* "reified" *) token env tok
  | `Vari_modi x -> map_type_projection_modifier env x
  | `Anno x -> map_annotation env x
  )

and map_type_parameter_modifiers (env : env) (xs : CST.type_parameter_modifiers) =
  List.map (map_type_parameter_modifier env) xs

and map_type_parameters (env : env) ((v1, v2, v3, v4) : CST.type_parameters) =
  let v1 = (* "<" *) token env v1 in
  let v2 = map_type_parameter env v2 in
  let v3 =
    List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_type_parameter env v2 in
      todo env (v1, v2)
    ) v3
  in
  let v4 = (* ">" *) token env v4 in
  todo env (v1, v2, v3, v4)

and map_type_projection (env : env) (x : CST.type_projection) =
  (match x with
  | `Opt_type_proj_modifs_type (v1, v2) ->
      let v1 =
        (match v1 with
        | Some x -> map_type_projection_modifiers env x
        | None -> todo env ())
      in
      let v2 = map_type_ env v2 in
      todo env (v1, v2)
  | `STAR tok -> (* "*" *) token env tok
  )

and map_type_reference (env : env) (x : CST.type_reference) =
  (match x with
  | `User_type x -> map_user_type env x
  | `Dyna tok -> (* "dynamic" *) token env tok
  )

and map_type_test (env : env) ((v1, v2) : CST.type_test) =
  let v1 = map_is_operator env v1 in
  let v2 = map_type_ env v2 in
  todo env (v1, v2)

and map_unary_expression (env : env) (x : CST.unary_expression) =
  (match x with
  | `Post_exp (v1, v2) ->
      let v1 = map_expression env v1 in
      let v2 = map_postfix_unary_operator env v2 in
      todo env (v1, v2)
  | `Call_exp (v1, v2) ->
      let v1 = map_expression env v1 in
      let v2 = map_call_suffix env v2 in
      todo env (v1, v2)
  | `Inde_exp (v1, v2) ->
      let v1 = map_expression env v1 in
      let v2 = map_indexing_suffix env v2 in
      todo env (v1, v2)
  | `Navi_exp (v1, v2) ->
      let v1 = map_expression env v1 in
      let v2 = map_navigation_suffix env v2 in
      todo env (v1, v2)
  | `Prefix_exp (v1, v2) ->
      let v1 =
        (match v1 with
        | `Anno x -> map_annotation env x
        | `Label tok -> (* label *) token env tok
        | `Prefix_un_op x -> map_prefix_unary_operator env x
        )
      in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
  | `As_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = map_as_operator env v2 in
      let v3 = map_type_ env v3 in
      todo env (v1, v2, v3)
  | `Spread_exp (v1, v2) ->
      let v1 = (* "*" *) token env v1 in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
  )

and map_unescaped_annotation (env : env) (x : CST.unescaped_annotation) =
  (match x with
  | `Cons_invo x -> map_constructor_invocation env x
  | `User_type x -> map_user_type env x
  )

and map_user_type (env : env) ((v1, v2) : CST.user_type) =
  let v1 = map_simple_user_type env v1 in
  let v2 =
    List.map (fun (v1, v2) ->
      let v1 = (* "." *) token env v1 in
      let v2 = map_simple_user_type env v2 in
      todo env (v1, v2)
    ) v2
  in
  todo env (v1, v2)

and map_value_argument (env : env) ((v1, v2, v3, v4) : CST.value_argument) =
  let v1 =
    (match v1 with
    | Some x -> map_annotation env x
    | None -> todo env ())
  in
  let v2 =
    (match v2 with
    | Some (v1, v2) ->
        let v1 = map_simple_identifier env v1 in
        let v2 = (* "=" *) token env v2 in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v3 =
    (match v3 with
    | Some tok -> (* "*" *) token env tok
    | None -> todo env ())
  in
  let v4 = map_expression env v4 in
  todo env (v1, v2, v3, v4)

and map_value_arguments (env : env) ((v1, v2, v3) : CST.value_arguments) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) ->
        let v1 = map_value_argument env v1 in
        let v2 =
          List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_value_argument env v2 in
            todo env (v1, v2)
          ) v2
        in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v3 = (* ")" *) token env v3 in
  todo env (v1, v2, v3)

and map_variable_declaration (env : env) ((v1, v2) : CST.variable_declaration) =
  let v1 = map_simple_identifier env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) ->
        let v1 = (* ":" *) token env v1 in
        let v2 = map_type_ env v2 in
        todo env (v1, v2)
    | None -> todo env ())
  in
  todo env (v1, v2)

and map_when_condition (env : env) (x : CST.when_condition) =
  (match x with
  | `Exp x -> map_expression env x
  | `Range_test x -> map_range_test env x
  | `Type_test x -> map_type_test env x
  )

and map_when_entry (env : env) ((v1, v2, v3, v4) : CST.when_entry) =
  let v1 =
    (match v1 with
    | `When_cond_rep_COMMA_when_cond (v1, v2) ->
        let v1 = map_when_condition env v1 in
        let v2 =
          List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_when_condition env v2 in
            todo env (v1, v2)
          ) v2
        in
        todo env (v1, v2)
    | `Else tok -> (* "else" *) token env tok
    )
  in
  let v2 = (* "->" *) token env v2 in
  let v3 = map_control_structure_body env v3 in
  let v4 =
    (match v4 with
    | Some tok -> (* automatic_semicolon *) token env tok
    | None -> todo env ())
  in
  todo env (v1, v2, v3, v4)

and map_when_subject (env : env) ((v1, v2, v3, v4) : CST.when_subject) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3, v4) ->
        let v1 = List.map (map_annotation env) v1 in
        let v2 = (* "val" *) token env v2 in
        let v3 = map_variable_declaration env v3 in
        let v4 = (* "=" *) token env v4 in
        todo env (v1, v2, v3, v4)
    | None -> todo env ())
  in
  let v3 = map_expression env v3 in
  let v4 = (* ")" *) token env v4 in
  todo env (v1, v2, v3, v4)

let map_import_list (env : env) ((v1, v2) : CST.import_list) =
  let v1 = List.map (map_import_header env) v1 in
  let v2 = (* import_list_delimiter *) token env v2 in
  todo env (v1, v2)

let map_file_annotation (env : env) ((v1, v2, v3, v4, v5) : CST.file_annotation) =
  let v1 = (* "@" *) token env v1 in
  let v2 = (* "file" *) token env v2 in
  let v3 = (* ":" *) token env v3 in
  let v4 =
    (match v4 with
    | `LBRACK_rep1_unes_anno_RBRACK (v1, v2, v3) ->
        let v1 = (* "[" *) token env v1 in
        let v2 = List.map (map_unescaped_annotation env) v2 in
        let v3 = (* "]" *) token env v3 in
        todo env (v1, v2, v3)
    | `Unes_anno x -> map_unescaped_annotation env x
    )
  in
  let v5 = (* automatic_semicolon *) token env v5 in
  todo env (v1, v2, v3, v4, v5)

let rec map_parenthesized_user_type (env : env) ((v1, v2, v3) : CST.parenthesized_user_type) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | `User_type x -> map_user_type env x
    | `Paren_user_type x -> map_parenthesized_user_type env x
    )
  in
  let v3 = (* ")" *) token env v3 in
  todo env (v1, v2, v3)

let map_source_file (env : env) (x : CST.source_file) =
  (match x with
  | `Opt_sheb_line_rep_file_anno_opt_pack_header_rep_import_list_rep_stmt_semi (v1, v2, v3, v4, v5) ->
      let v1 =
        (match v1 with
        | Some x -> map_shebang_line env x
        | None -> todo env ())
      in
      let v2 = List.map (map_file_annotation env) v2 in
      let v3 =
        (match v3 with
        | Some x -> map_package_header env x
        | None -> todo env ())
      in
      let v4 = List.map (map_import_list env) v4 in
      let v5 =
        List.map (fun (v1, v2) ->
          let v1 = map_statement env v1 in
          let v2 = (* automatic_semicolon *) token env v2 in
          todo env (v1, v2)
        ) v5
      in
      todo env (v1, v2, v3, v4, v5)
  | `Semg_exp (v1, v2) ->
      let v1 = (* "__SEMGREP_EXPRESSION" *) token env v1 in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
  )
