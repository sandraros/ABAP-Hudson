*&---------------------------------------------------------------------*
*&  Include           ZCAGS_CI_SCREEN_HANDLER
*&---------------------------------------------------------------------*
CLASS lcl_ci_screen_handler DEFINITION.
  PUBLIC SECTION.

    METHODS load_packages_to_selection
      IMPORTING
        it_packages TYPE tr_devclasses.


    METHODS run_unit_tests.
ENDCLASS.                    "lcl_ci_screen_handler DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_ci_screen_handler IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_ci_screen_handler IMPLEMENTATION.

  METHOD load_packages_to_selection.

    DATA ls_custom_package TYPE devclass.

    REFRESH pr_pckg.
    pr_pckg-option  = 'EQ'.
    pr_pckg-sign    = 'I'.

    LOOP AT gt_root_custom_packages INTO ls_custom_package.
      pr_pckg-low     = ls_custom_package.
      APPEND pr_pckg TO pr_pckg.
    ENDLOOP.

  ENDMETHOD.                    "load_packages_to_selection


  METHOD run_unit_tests.

    zcl_cags_ci_code_coverage=>free_memory( ).
    zcl_cags_ci_report=>free_config_memory( ).

    DATA lo_unit_tests_report TYPE REF TO zcl_cags_ci_report.
    lo_unit_tests_report = zcl_cags_ci_report=>get_instance( ).

    lo_unit_tests_report->init(
      i_is_report_run = abap_true
      i_classes_coverage_only = boolc( g_coverage_class_only = abap_true )
      i_group_by_user = g_group_by_user
      i_group_by_package = g_group_by_package
      i_test_user_file = p_tst_u
      i_test_package_file = p_tst_p
      i_coverage_user_file = p_cov_u
      i_coverage_package_file = p_cov_p
      i_local_sap_dir = p_dir_l
      i_remote_dir = p_dir_r
      i_email_for_failures = p_mail
    ).
    lo_unit_tests_report->save_to_memory( ).

    lo_unit_tests_report->delete_sap_files( ).
    lo_unit_tests_report->lock_run( ).

    sy-ucomm = 'ONLI'.

    SUBMIT rs_aucv_runner                 "#EC CI_SUBMIT
        WITH so_devc IN pr_pckg           " list of packages
        WITH b_email = ' '                " do not send mails opiton
        WITH b_direct = 'X'               " show results in ALV
        WITH p_aucv = 'X'                 " With code coverage
        WITH p_packr = 'X'                " with subpackages
        AND RETURN
      .

    zcl_cags_ci_code_coverage=>free_memory( ).
    zcl_cags_ci_report=>free_config_memory( ).

    lo_unit_tests_report->unlock_run( ).

  ENDMETHOD.                    "run_unit_tests

ENDCLASS.                    "lcl_ci_screen_handler IMPLEMENTATION
