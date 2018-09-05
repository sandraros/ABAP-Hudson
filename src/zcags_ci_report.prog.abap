*&---------------------------------------------------------------------*
*& Report  ZCAGS_CI_REPORT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zcags_ci_report.
INCLUDE zcags_ci_top.
INCLUDE zcags_ci_sel_screen.
INCLUDE zcags_ci_screen_handler.



DATA lo_ci_screen_handler             TYPE REF TO lcl_ci_screen_handler.

INITIALIZATION.

  CREATE OBJECT lo_ci_screen_handler.
  gt_root_custom_packages = zcl_cags_ci_metadata_dao=>get_instance( )->find_custom_root_packages( ).

START-OF-SELECTION.

  lo_ci_screen_handler->run_unit_tests( ).

AT SELECTION-SCREEN OUTPUT.

  g_packages_custom_auto_load      = abap_false.
  g_packages_customized            = abap_false.
  g_coverage_class_only            = abap_false.
  g_coverage_class_and_program     = abap_false.
  g_group_by_package               = abap_false.
  g_group_by_user                  = abap_false.

  IF ( pr_1_1 = 'X' ).
    g_packages_custom_auto_load     = abap_true.
  ELSEIF ( pr_1_2 = 'X' ).
    g_packages_customized           = abap_true.
  ENDIF.

  IF ( pr_2_1 = 'X' ).
    g_coverage_class_only           = abap_true.
  ELSEIF ( pr_2_2 = 'X' ).
    g_coverage_class_and_program    = abap_true.
  ENDIF.

  IF ( pr_3_1 = 'X' ).
    g_group_by_user              = abap_true.
    g_group_by_package              = abap_true.
  ELSEIF ( pr_3_2 = 'X' ).
    g_group_by_user                 = abap_true.
  ELSEIF ( pr_3_3 = 'X' ).
    g_group_by_package              = abap_true.
  ENDIF.

  IF ( g_packages_custom_auto_load = abap_true ).

    lo_ci_screen_handler->load_packages_to_selection( gt_root_custom_packages ).

  ENDIF.
