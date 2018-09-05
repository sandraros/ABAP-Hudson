*&---------------------------------------------------------------------*
*&  Include           ZCAGS_CI_TOP
*&---------------------------------------------------------------------*
TABLES tdevc.
DATA gt_root_custom_packages TYPE tr_devclasses.

DATA g_packages_custom_auto_load      TYPE abap_bool.
DATA g_packages_customized            TYPE abap_bool.
DATA g_coverage_class_only            TYPE abap_bool.
DATA g_coverage_class_and_program     TYPE abap_bool.
DATA g_group_by_package               TYPE abap_bool.
DATA g_group_by_user                  TYPE abap_bool.
