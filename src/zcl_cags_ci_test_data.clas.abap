*----------------------------------------------------------------------*
*       CLASS ZCL_CAGS_CI_TEST_DATA DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class ZCL_CAGS_CI_TEST_DATA definition
  public
  create public .

public section.
  type-pools ABAP .

  class-methods FILTER_CLASS_NAME
    importing
      !I_CLASS_NAME type STRING
    returning
      value(R_PURE_CLASS_NAME) type STRING .
  class-methods GET_INSTANCE
    returning
      value(RO_TEST_DATA_INSTANCE) type ref to ZCL_CAGS_CI_TEST_DATA .
  methods ADD_FAILED_METHOD
    importing
      !I_CLASS_NAME type STRING
      !I_LOCAL_TEST_CLASS type STRING
      !I_METHOD_NAME type STRING
      !I_ALERT_MSG type STRING .
  methods ADD_TEST_METHOD
    importing
      !I_METHOD_NAME type STRING
      !I_LOCAL_TEST_CLASS type STRING
      !I_CLASS_NAME type STRING .
  methods CONSTRUCTOR .
  methods FILTER_METHODS_REMOVE_FAILED
    importing
      !IT_FAILED_METHODS type ZCAGS_CI_UNIT_METHOD_TT
      !IT_ALL_METHODS type ZCAGS_CI_UNIT_METHOD_TT
    returning
      value(RT_SUCCESS_METHODS) type ZCAGS_CI_UNIT_METHOD_TT .
  class-methods SET_INSTANCE
    importing
      !IO_NEW_INSTANCE type ref to ZCL_CAGS_CI_TEST_DATA .
  methods BUILD_XML_TESTS_RESULT
    exporting
      !ET_TESTS_RESULTS_PACKAGE type TCHAR255
      !ET_TESTS_RESULTS_USER type TCHAR255 .
  methods GET_FAILED_METHODS
    returning
      value(RT_FAILED_METHODS) type ZCAGS_CI_UNIT_METHOD_TT .
PROTECTED SECTION.
PRIVATE SECTION.

    CLASS-DATA mo_test_data_instance TYPE REF TO zcl_cags_ci_test_data .
    CLASS-DATA c_log_file_name TYPE string VALUE 'CI_DEBUG.LOG'. "#EC NOTEXT .  . " .

    CLASS-DATA mo_metadata_dao TYPE REF TO zcl_cags_ci_metadata_dao.

    DATA mt_all_methods TYPE zcags_ci_unit_method_tt .
    DATA mt_failed_methods TYPE zcags_ci_unit_method_tt .
    CLASS-DATA c_abap_unit_xml_file_package TYPE string VALUE 'CI_ABAP_UNIT_PACKAGE.XML'. "#EC NOTEXT . " .
    CLASS-DATA c_abap_unit_xml_file_user TYPE string VALUE 'CI_ABAP_UNIT_USER.XML'. "#EC NOTEXT .

    CLASS-METHODS filter_method_name
      IMPORTING
        !i_method_name TYPE string
      RETURNING
        value(r_pure_method_name) TYPE string .
ENDCLASS.



CLASS ZCL_CAGS_CI_TEST_DATA IMPLEMENTATION.


METHOD add_failed_method.

    DATA ls_method TYPE zcags_ci_unit_method_s.
    DATA l_filtered_method_name TYPE string.
    DATA l_filtered_class_name TYPE string.
    DATA l_class_user TYPE uname.
    DATA l_class_package TYPE devclass.

    IF ( i_class_name IS INITIAL OR i_class_name = '???' ).
      l_filtered_class_name = filter_class_name( i_method_name ).
    ELSE.
      l_filtered_class_name = filter_class_name( i_class_name ).
    ENDIF.

    IF ( l_filtered_class_name IS INITIAL ).
      l_filtered_class_name = 'CLASS_UNKOWN'.
    ENDIF.

    l_filtered_method_name = filter_method_name( i_method_name ).

    l_class_user = mo_metadata_dao->find_class_user( l_filtered_class_name ).

    l_class_package = mo_metadata_dao->find_class_package_full_path( l_filtered_class_name ).

    ls_method-class_name  = l_filtered_class_name && '~' && i_local_test_class.
    ls_method-method_name = l_filtered_method_name.
    ls_method-alert_info  = i_alert_msg.
    ls_method-author_user = l_class_user.
    ls_method-class_package = l_class_package.
    APPEND ls_method TO mt_failed_methods.



  ENDMETHOD.                    "add_failed_method


METHOD add_test_method.

    DATA ls_method TYPE zcags_ci_unit_method_s.
    DATA l_filtered_method_name TYPE string.
    DATA l_filtered_class_name TYPE string.
    DATA l_class_user TYPE uname.
    DATA l_class_package TYPE string.
    l_filtered_class_name = filter_class_name( i_class_name ).
    l_filtered_method_name = filter_method_name( i_method_name ).
    l_class_user = mo_metadata_dao->find_class_user( l_filtered_class_name ).
    l_class_package = mo_metadata_dao->find_class_package_full_path( l_filtered_class_name ).

    ls_method-class_name = l_filtered_class_name && '~' && i_local_test_class.
    ls_method-method_name = l_filtered_method_name.
    ls_method-author_user = l_class_user.
    ls_method-class_package = l_class_package.
    APPEND ls_method TO mt_all_methods.

  ENDMETHOD.                    "add_test_method


METHOD build_xml_tests_result.

    DATA ls_method TYPE zcags_ci_unit_method_s.
    DATA lt_success_methods TYPE zcags_ci_unit_method_tt.
    DATA l_class_name TYPE string.
    DATA l_method_name TYPE string.
    DATA l_alert_info TYPE string.
    DATA l_package TYPE string.
    DATA l_class_user TYPE uname.

    DATA lo_junit_xml_creator TYPE REF TO zcl_cags_ci_test_xml_creator.
    CREATE OBJECT lo_junit_xml_creator.

    lo_junit_xml_creator->open( ).

    LOOP AT mt_failed_methods INTO ls_method.
      l_method_name = ls_method-method_name.
      l_class_name = ls_method-class_name.
      l_alert_info = ls_method-alert_info.
      l_class_user = ls_method-author_user.
      l_package    = ls_method-class_package.

      lo_junit_xml_creator->add_failure_method(
          EXPORTING
            i_package     = l_package
            i_user        = l_class_user
            i_class_name  = l_class_name
            i_method_name = l_method_name
            i_failure_msg = l_alert_info
            i_failure_type = 'ABAP Unit Test error'
        ).

    ENDLOOP.

    lt_success_methods = filter_methods_remove_failed(
      it_failed_methods = mt_failed_methods
      it_all_methods    = mt_all_methods
    ).
    LOOP AT lt_success_methods INTO ls_method.

      l_method_name = ls_method-method_name.
      l_class_name = ls_method-class_name.
      l_class_user = ls_method-author_user.
      l_package = ls_method-class_package.

      lo_junit_xml_creator->add_success_method(
          EXPORTING
            i_package     = l_package
            i_user        = l_class_user
            i_class_name  = l_class_name
            i_method_name = l_method_name
        ).

    ENDLOOP.

    lo_junit_xml_creator->close( ).

    et_tests_results_package = lo_junit_xml_creator->get_xml_content_package( ).
    et_tests_results_user = lo_junit_xml_creator->get_xml_content_user( ).


  ENDMETHOD.                    "build_xml_tests_result


METHOD constructor.

    mo_metadata_dao = zcl_cags_ci_metadata_dao=>get_instance( ).

  ENDMETHOD.                    "constructor


METHOD filter_class_name.

    DATA l_name_length TYPE i.
    DATA l_class_name_30_char TYPE char30.
    DATA l_equality_chars_postfix TYPE string.
    l_name_length = strlen( i_class_name ).

    IF ( zcl_util_string=>starts_with(
        i_string      = i_class_name
        i_prefix      = '\XX=YY\CLASS='
     ) = abap_true ).
      DATA l_class_prefix TYPE string.
      DATA l_class_suffix TYPE string.
      SPLIT i_class_name AT '\XX=YY\CLASS=' INTO l_class_prefix l_class_suffix.
      SPLIT l_class_suffix AT '\METHOD=' INTO l_class_prefix l_class_suffix.
      r_pure_class_name = l_class_prefix.
    ELSEIF ( l_name_length > 30 ).
      l_class_name_30_char = zcl_util_string=>substring(
          i_string = i_class_name
          i_length = 30
      ).

      SPLIT l_class_name_30_char AT '=' INTO r_pure_class_name l_equality_chars_postfix.
    ELSE.
      r_pure_class_name = i_class_name.
    ENDIF.

  ENDMETHOD.                    "filter_class_name


METHOD filter_methods_remove_failed.

    DATA ls_failed_method TYPE zcags_ci_unit_method_s.

    rt_success_methods = it_all_methods.

    LOOP AT it_failed_methods INTO ls_failed_method.
      DELETE rt_success_methods WHERE class_name = ls_failed_method-class_name AND method_name = ls_failed_method-method_name.
    ENDLOOP.

  ENDMETHOD.                    "filter_methods_remove_failed


METHOD filter_method_name.

    DATA l_method_string_prefix TYPE string.
    DATA l_method_name TYPE string.

    "\XX=YY\CLASS=LCL_CALCULATOR2_TEST\METHOD=ADD2
    SPLIT i_method_name AT 'METHOD=' INTO l_method_string_prefix l_method_name.

    IF ( l_method_name IS INITIAL ).
      r_pure_method_name = l_method_string_prefix.
    ELSE.
      r_pure_method_name = l_method_name.
    ENDIF.

  ENDMETHOD.                    "filter_method_name


METHOD get_failed_methods.

    rt_failed_methods = mt_failed_methods.

  ENDMETHOD.                    "get_failed_methods


METHOD get_instance.

    IF ( mo_test_data_instance IS INITIAL ).
      CREATE OBJECT mo_test_data_instance.
    ENDIF.

    ro_test_data_instance = mo_test_data_instance.

  ENDMETHOD.                    "GET_INSTANCE


METHOD set_instance.

    mo_test_data_instance = io_new_instance.

  ENDMETHOD.                    "set_instance
ENDCLASS.
