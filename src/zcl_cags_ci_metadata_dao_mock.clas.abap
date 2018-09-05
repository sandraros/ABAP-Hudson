*----------------------------------------------------------------------*
*       CLASS zcl_cags_ci_metadata_dao_mock DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS zcl_cags_ci_metadata_dao_mock DEFINITION INHERITING FROM zcl_cags_ci_metadata_dao
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS c_test_coverage_key1 TYPE saunit_d_coverage_analysis_key VALUE 'CKEY'.
    CONSTANTS c_test_coverage_user1 TYPE sy-uname VALUE 'USER_01'.
    CONSTANTS c_test_coverage_package1 TYPE zcags_ci_code_coverage_s-parent_object VALUE 'PACKAGE_01'.
    CONSTANTS c_test_coverage_object1 TYPE zcags_ci_code_coverage_s-name VALUE 'ZCL_TEST_CLASS_NAME'.

    METHODS find_class_user REDEFINITION.

    METHODS find_class_PACKAGE_full_path REDEFINITION.

    METHODS find_full_PACKAGE_path REDEFINITION.

    METHODS find_class_metadata REDEFINITION.

    METHODS find_custom_root_packages REDEFINITION.

    METHODS find_sap_coverage_results REDEFINITION.

    METHODS find_full_PACKAGE_path_child REDEFINITION.
PROTECTED SECTION.
PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_CAGS_CI_METADATA_DAO_MOCK IMPLEMENTATION.


METHOD find_class_metadata.

    DATA l_class_name TYPE seoclass-clsname.

    IF ( i_class_name = c_test_coverage_object1 AND i_package = c_test_coverage_package1 ).

      e_user = c_test_coverage_user1.
      e_is_deleted = abap_false.

    ENDIF.

  ENDMETHOD.                    "find_class_metadata


METHOD find_class_PACKAGE_full_path.

    IF ( i_class_name = 'ZCL_TEST_CLASS1' OR i_class_name = 'ZCL_TEST_CLASS2' ).
      r_class_package = 'TEST.PACKAGE_1'.
    ELSEIF ( i_class_name = 'ZCL_TEST_CLASS3' ).
      r_class_package = 'TEST.PACKAGE_3'.
    ENDIF.

  ENDMETHOD.                    "find_class_PACKAGE_full_path


METHOD find_class_user.
    IF ( i_class_name = 'ZCL_TEST_CLASS1' OR i_class_name = 'ZCL_TEST_CLASS2' ).
      r_class_author_user = 'TEST_USER1'.
    ELSEIF ( i_class_name = 'ZCL_TEST_CLASS3' ).
      r_class_author_user = 'TEST_USER3'.
    ENDIF.
  ENDMETHOD.                    "find_class_user


METHOD find_custom_root_packages.

  ENDMETHOD.                    "find_custom_root_packages


METHOD find_full_PACKAGE_path.
    IF ( i_PACKAGE_name = 'PACKAGE_1' OR i_PACKAGE_name = 'PACKAGE_3' ).
      r_PACKAGE_full_path = 'TEST.' && i_PACKAGE_name.
    ENDIF.

  ENDMETHOD.                    "find_full_PACKAGE_path


METHOD find_full_PACKAGE_path_child.

  ENDMETHOD.                    "find_full_PACKAGE_path_child


METHOD find_sap_coverage_results.

    IF ( i_coverage_key = c_test_coverage_key1 ).

      DATA lt_sap_coverage_results  TYPE cvt_propro_tkey.
      DATA ls_sap_coverage_result   TYPE cvs_propro_tkey.

      ls_sap_coverage_result-obj_name = 'SAP_BUT_NOT_CUST_OBJ'.
      APPEND ls_sap_coverage_result TO lt_sap_coverage_results.

      CLEAR ls_sap_coverage_result.
      ls_sap_coverage_result-devclass = c_test_coverage_package1.
      ls_sap_coverage_result-obj_name = c_test_coverage_object1.
      ls_sap_coverage_result-proz_cnt_cum = '2'.    " method coverage percent
      ls_sap_coverage_result-total        = '200'.  " methods total count
      ls_sap_coverage_result-proz_cnt_cum_brch = '3'. " blocks coverage percent
      ls_sap_coverage_result-branch_count = '300'.    " blocks coverage total amount
      ls_sap_coverage_result-proz_cnt_cum_st = '4'.  " statements/lines coverage percent
      ls_sap_coverage_result-statement_count = '400'. " statements/lines total amount in coverage

      APPEND ls_sap_coverage_result TO lt_sap_coverage_results.

      rt_sap_coverage_result = lt_sap_coverage_results.

    ENDIF.

  ENDMETHOD.                    "find_sap_coverage_results
ENDCLASS.
