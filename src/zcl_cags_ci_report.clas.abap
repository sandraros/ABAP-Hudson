*----------------------------------------------------------------------*
*       CLASS ZCL_CAGS_CI_REPORT DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*       CLASS zcl_cags_ci_report DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class ZCL_CAGS_CI_REPORT definition
  public
  create public .

public section.
  type-pools ABAP .

  constants C_DEFAULT_SERVER_DIR type STRING value '\\SAP-IFACE01.ST.STATOIL.NO\TRANS\ERPDEV\' ##NO_TEXT.
  constants C_DEFLT_COVERAGE_USER_FILE type FILE_NAME value 'CI_ABAP_COVERAGE_USER.XML' ##NO_TEXT.
  constants C_DEFLT_COVERAGE_PACKAGE_FILE type FILE_NAME value 'CI_ABAP_COVERAGE_PACKAGE.XML' ##NO_TEXT.
  constants C_DEFLT_TEST_PACKAGE_FILE type FILE_NAME value 'CI_ABAP_UNIT_PACKAGE.XML' ##NO_TEXT.
  constants C_DEFLT_TEST_USER_FILE type FILE_NAME value 'CI_ABAP_UNIT_USER.XML' ##NO_TEXT.
  constants C_BATCH_JOBNAME type BTCJOB value 'AUTO_ZCAGS_CI_REPORT' ##NO_TEXT.
  constants C_DEFAULT_VARIANT type CHAR20 value 'HTTP_DEFAULT' ##NO_TEXT.
  data M_REPORT_EXEC_USER type SY-UNAME value '774888' ##NO_TEXT.

  class-methods GET_INSTANCE
    returning
      value(RO_SINGLETON_INSTANCE) type ref to ZCL_CAGS_CI_REPORT .
  class-methods FREE_CONFIG_MEMORY .
  methods CONSTRUCTOR .
  methods INIT
    importing
      !I_IS_REPORT_RUN type ABAP_BOOL optional
      !I_CLASSES_COVERAGE_ONLY type ABAP_BOOL optional
      !I_GROUP_BY_USER type ABAP_BOOL optional
      !I_GROUP_BY_PACKAGE type ABAP_BOOL optional
      !I_TEST_USER_FILE type STRING optional
      !I_TEST_PACKAGE_FILE type STRING optional
      !I_COVERAGE_USER_FILE type STRING optional
      !I_COVERAGE_PACKAGE_FILE type STRING optional
      !I_LOCAL_SAP_DIR type STRING optional
      !I_REMOTE_DIR type STRING optional
      !I_EMAIL_FOR_FAILURES type ABAP_BOOL optional .
  methods IS_ON
    returning
      value(R_IS_ON) type ABAP_BOOL .
  methods IS_COVERAGE_ONLY_FOR_CLASSES
    returning
      value(R_ONLY_CLASSES) type ABAP_BOOL .
  methods READ_FROM_MEMORY .
  methods SAVE_TO_MEMORY .
  methods GET_CONFIGURATION
    returning
      value(RS_CONFIGURATION) type ZCAGS_CI_CONFIGURATION .
  methods UPDATE_FINAL_COVERAGE_RESULTS
    returning
      value(RO_COVERAGE_RESULTS) type ref to ZCL_CAGS_CI_CODE_COVERAGE .
  methods UPDATE_FINAL_TESTS_RESULTS
    importing
      !IT_SAP_PROGRAMS_TEST_RESULT type IF_SAUNIT_INTERNAL_RESULT_TYPE=>TY_T_PROGRAMS
    returning
      value(RO_TEST_DATA) type ref to ZCL_CAGS_CI_TEST_DATA .
  methods IS_CUSTOMIZED_NAME
    importing
      !I_OBJECT_NAME type STRING
    returning
      value(R_IS_CUSTOMIZED_NAME) type ABAP_BOOL .
  methods SAVE_TO_LOCAL_SAP_DIR
    importing
      !I_FILEPATH type FILE_NAME
      !IT_FILE_CONTENT_LINES type TCHAR255 .
  methods TRANSFER_DATA_TO_HUDSON
    importing
      !I_REMOTE_FILE_PATH type FILE_NAME
    changing
      !IT_FILE_CONTENT_LINES type TCHAR255 .
  methods CONCATENATE_FILE_DIR_TO_PATH
    importing
      !I_FILE_NAME type FILE_NAME
      !I_DIR_PATH type FILE_NAME
    returning
      value(R_FILE_PATH) type FILE_NAME .
  methods UPDATE_SUMMARY_SAVE_FILES
    importing
      !IT_SAP_PROGRAMS_TEST_RESULT type IF_SAUNIT_INTERNAL_RESULT_TYPE=>TY_T_PROGRAMS .
  methods LOCK_RUN .
  methods UNLOCK_RUN
    importing
      !I_SYSTEM_ID type SY-SYSID default SY-SYSID .
  methods IS_RUN_LOCK_FILE
    returning
      value(R_LOCK_FILE_FOUND) type ABAP_BOOL .
  methods CHECK_IF_TEST_FILE_EXISTS
    importing
      !I_FILE_NAME type STRING
    returning
      value(R_RESULT) type ABAP_BOOL .
  methods CREATE_SAP_FILE_PATH
    importing
      !I_FILE_NAME type FILE_NAME
      !I_SYSTEM_ID type SY-SYSID default SY-SYSID
    returning
      value(R_FILE_FULL_PATH) type FILE_NAME .
  methods CREATE_HUDSON_FILE_PATH
    importing
      !I_FILE_NAME type FILE_NAME
    returning
      value(R_FILE_FULL_PATH) type FILE_NAME .
  class-methods DELETE_FILE
    importing
      !I_FILE_PATH type STRING .
  methods DELETE_SAP_FILES
    importing
      !I_SYSTEM_ID type SY-SYSID default SY-SYSID .
  methods INIT_DEFAULT_FILES_PATH .
  methods GET_SAP_DIR
    returning
      value(R_DIRPATH) type FILE_NAME .
  methods RUN_UNIT_TESTS_DEFAULT_VARIANT .
  methods SEND_EMAIL_FOR_FAILURES
    importing
      !IO_TEST_RESULTS type ref to ZCL_CAGS_CI_TEST_DATA .
  methods SEND_EMAIL_TO_USER
    importing
      !I_USER type SY-UNAME
      !I_FAILED_CLASS_NAME type ZCAGS_CI_UNIT_METHOD_S-CLASS_NAME .
  methods SET_REPORT_EXEC_USER
    importing
      !I_USER type SY-UNAME default '123456' .
PROTECTED SECTION.
PRIVATE SECTION.


    CONSTANTS c_memory_configuration_id   TYPE char32 VALUE 'ZCL_CAGS_CI_REPORT-CONFIGURATION'.
    CLASS-DATA m_memory_configuration_id  TYPE char40 VALUE 'ZCL_CAGS_CI_REPORT-CONFIGURATION'.

    CONSTANTS c_file_lock_suffix TYPE file_name VALUE 'run.lock'.

    CLASS-DATA mo_singleton_instance TYPE REF TO zcl_cags_ci_report .
    CLASS-DATA mo_metadata_dao TYPE REF TO zcl_cags_ci_metadata_dao.

    DATA ms_report_configuration TYPE zcags_ci_configuration.
ENDCLASS.



CLASS ZCL_CAGS_CI_REPORT IMPLEMENTATION.


METHOD check_if_test_file_exists.

    r_result = abap_false.
    OPEN DATASET i_file_name FOR INPUT IN TEXT MODE ENCODING DEFAULT WITH WINDOWS LINEFEED.
    IF ( sy-subrc = 0 ).
      r_result = abap_true.
    ENDIF.

  ENDMETHOD.                    "check_if_test_file_exists


METHOD concatenate_file_dir_to_path.

    DATA l_dir_length TYPE i.
    DATA l_dir_last_char_pos TYPE i.
    DATA l_last_char TYPE char1.
    DATA l_dir_formatted TYPE string.
    l_dir_length = strlen( i_dir_path ).

    IF ( l_dir_length > 0 ).
      l_dir_last_char_pos = l_dir_length - 1.
      l_last_char = i_dir_path+l_dir_last_char_pos(1).
      IF ( l_last_char <> '\'  AND l_last_char <> '/' ).
        l_dir_formatted = i_dir_path && '\'.
      ELSE.
        l_dir_formatted = i_dir_path.
      ENDIF.
    ELSE.
      l_dir_formatted = i_dir_path.
    ENDIF.

    r_file_path = l_dir_formatted && i_file_name.

  ENDMETHOD.                    "concatenate_file_dir_to_path


METHOD constructor.

    mo_metadata_dao = zcl_cags_ci_metadata_dao=>get_instance( ).

    m_memory_configuration_id = c_memory_configuration_id && sy-sysid.
    m_report_exec_user = sy-uname.

  ENDMETHOD.                    "constructor


METHOD create_hudson_file_path.

    r_file_full_path = ms_report_configuration-remote_hudson_dir && sy-sysid && '_' && i_file_name.

  ENDMETHOD.                    "create_sap_file_path


METHOD create_sap_file_path.

    r_file_full_path = ms_report_configuration-local_sap_dir && i_system_id && '_' && i_file_name.

  ENDMETHOD.                    "create_sap_file_path


METHOD delete_file.

    DELETE DATASET i_file_path.

  ENDMETHOD.                    "delete_sap_files


METHOD delete_sap_files.

    DATA l_system_id TYPE sy-sysid.
    l_system_id = i_system_id.
    IF ( l_system_id IS INITIAL ).
      l_system_id = sy-sysid.
    ENDIF.

    delete_file( create_sap_file_path( i_file_name = ms_report_configuration-test_user_file i_system_id = l_system_id ) ).
    delete_file( create_sap_file_path( i_file_name = ms_report_configuration-test_package_file i_system_id = l_system_id ) ).
    delete_file( create_sap_file_path( i_file_name = ms_report_configuration-coverage_user_file i_system_id = l_system_id ) ).
    delete_file( create_sap_file_path( i_file_name = ms_report_configuration-coverage_package_file i_system_id = l_system_id ) ).

  ENDMETHOD.                    "action_del_file


METHOD free_config_memory.
    FREE MEMORY ID m_memory_configuration_id.
  ENDMETHOD.                    "free_memory


METHOD get_configuration.
    rs_configuration = ms_report_configuration.
  ENDMETHOD.                    "get_configuration


METHOD get_instance.

    IF ( mo_singleton_instance IS INITIAL ).
      CREATE OBJECT mo_singleton_instance.
      mo_singleton_instance->read_from_memory( ).
    ENDIF.

    ro_singleton_instance = mo_singleton_instance.

  ENDMETHOD.                    "GET_INSTANCE


METHOD get_sap_dir.

    r_dirpath = ms_report_configuration-local_sap_dir.

  ENDMETHOD.                    "get_sap_dir


METHOD init.

    ms_report_configuration-is_report_run = i_is_report_run.
    ms_report_configuration-is_classes_coverage_only = i_classes_coverage_only.
    ms_report_configuration-is_group_by_user = i_group_by_user.
    ms_report_configuration-is_group_by_package = i_group_by_package.
    ms_report_configuration-test_user_file = i_test_user_file.
    ms_report_configuration-test_package_file = i_test_package_file.
    ms_report_configuration-coverage_user_file = i_coverage_user_file.
    ms_report_configuration-coverage_package_file = i_coverage_package_file.
    ms_report_configuration-local_sap_dir = i_local_sap_dir.
    ms_report_configuration-remote_hudson_dir = i_remote_dir.
    ms_report_configuration-email_for_failures = i_email_for_failures.

  ENDMETHOD.                    "init


METHOD init_default_files_path.

    me->init(
          i_test_user_file        = sy-sysid && '_' && c_deflt_test_user_file
          i_test_package_file     = sy-sysid && '_' && c_deflt_test_package_file
          i_coverage_user_file    = sy-sysid && '_' && c_deflt_coverage_user_file
          i_coverage_package_file = sy-sysid && '_' && c_deflt_coverage_package_file
          i_local_sap_dir         = c_default_server_dir
        ).

  ENDMETHOD.                    "init_default_files_path


METHOD is_coverage_only_for_classes.

    r_only_classes = ms_report_configuration-is_classes_coverage_only.

  ENDMETHOD.                    "is_coverage_only_for_classes


METHOD is_customized_name.

    IF ( i_object_name IS INITIAL ).
      r_is_customized_name = abap_false.
    ELSEIF ( i_object_name+0(1) = 'Z' OR i_object_name+0(1) = 'Y' ).
      r_is_customized_name = abap_true.
    ELSE.
      r_is_customized_name = abap_false.
    ENDIF.

  ENDMETHOD.                    "IS_CUSTOMIZED_NAME


METHOD is_on.

    r_is_on = boolc( ms_report_configuration-is_report_run = abap_true ).

  ENDMETHOD.                    "IS_ON


METHOD is_run_lock_file.

    DATA l_system_lock_file TYPE string.
    l_system_lock_file = create_sap_file_path( c_file_lock_suffix ).
    r_lock_file_found = check_if_test_file_exists( l_system_lock_file ).

  ENDMETHOD.                    "is_run_lock_file


METHOD lock_run.

    DATA l_file_name TYPE string.
    l_file_name = create_sap_file_path( c_file_lock_suffix ).

    OPEN DATASET l_file_name FOR OUTPUT IN TEXT MODE ENCODING DEFAULT WITH WINDOWS LINEFEED.
    TRANSFER sy-uname TO l_file_name.
    TRANSFER sy-sysid TO l_file_name.
    TRANSFER sy-datum TO l_file_name.
    TRANSFER sy-uzeit TO l_file_name.
    CLOSE DATASET l_file_name.

    IF ( sy-subrc <> 0 ).

      zcx_cags_ci_report_exception=>raise_exception(
        i_code = sy-subrc
        i_msg  = 'Cannot create lock file:' && l_file_name ).

    ENDIF.

  ENDMETHOD.                    "lock_run


METHOD read_from_memory.

    CLEAR ms_report_configuration.
    IMPORT c_mem_report_configuration TO ms_report_configuration FROM MEMORY ID m_memory_configuration_id.

  ENDMETHOD.                    "READ_FROM_MEMORY


METHOD run_unit_tests_default_variant.

    DATA: l_jobcount TYPE btcjobcnt.
    DATA: l_batch_job_name TYPE btcjob.
    l_batch_job_name = sy-sysid && '_' && c_batch_jobname.

    l_jobcount = 001.

    CALL FUNCTION 'JOB_OPEN'
      EXPORTING
        delanfrep        = ' '
        jobgroup         = ' '
        jobname          = l_batch_job_name
        sdlstrtdt        = sy-datum
        sdlstrttm        = sy-uzeit
      IMPORTING
        jobcount         = l_jobcount
      EXCEPTIONS
        cant_create_job  = 21
        invalid_job_data = 22
        jobname_missing  = 23.

    IF sy-subrc NE 0.

      zcx_cags_ci_report_exception=>raise_exception(
        i_code = sy-subrc
        i_msg = 'ERROR: Cannot open batch job, error=' && sy-subrc
      ).
    ENDIF.

    SUBMIT zcags_ci_report AND RETURN   "#EC CI_SUBMIT
          USING SELECTION-SET c_default_variant
          USER m_report_exec_user
          VIA JOB l_batch_job_name
          NUMBER l_jobcount.

    IF sy-subrc NE 0.

      zcx_cags_ci_report_exception=>raise_exception(
          i_code = sy-subrc
          i_msg = 'ERROR: Cannot submit batch job, error=' && sy-subrc
        ).

    ENDIF.

    CALL FUNCTION 'JOB_CLOSE'
      EXPORTING
        jobcount             = l_jobcount
        jobname              = l_batch_job_name
        strtimmed            = 'X'
      EXCEPTIONS
        cant_start_immediate = 31
        invalid_startdate    = 32
        jobname_missing      = 33
        job_close_failed     = 34
        job_nosteps          = 35
        job_notex            = 36
        lock_failed          = 37
        OTHERS               = 39.

    IF sy-subrc NE 0.

      zcx_cags_ci_report_exception=>raise_exception(
        i_code = sy-subrc
        i_msg = 'ERROR: Cannot run batch job, error=' && sy-subrc
      ).

    ENDIF.

  ENDMETHOD.                    "run_unit_tests_default_variant


METHOD save_to_local_sap_dir.

    DATA l_xml_content_line TYPE char255.
    OPEN DATASET i_filepath FOR OUTPUT IN TEXT MODE ENCODING DEFAULT WITH WINDOWS LINEFEED.
    LOOP AT it_file_content_lines INTO l_xml_content_line.
      TRANSFER l_xml_content_line TO i_filepath.
    ENDLOOP.
    CLOSE DATASET i_filepath.

  ENDMETHOD.                    "save_to_local_sap_dir


METHOD save_to_memory.

    EXPORT c_mem_report_configuration FROM ms_report_configuration TO MEMORY ID m_memory_configuration_id.

  ENDMETHOD.                    "save_to_memory


METHOD send_email_for_failures.

    DATA lt_failed_methods TYPE zcags_ci_unit_method_tt.
    DATA ls_failed_method TYPE zcags_ci_unit_method_s.
    DATA l_user TYPE sy-uname.
    DATA l_global_class TYPE string.
    DATA l_global_local_class_name TYPE string.

    lt_failed_methods = io_test_results->get_failed_methods( ).

    LOOP AT lt_failed_methods INTO ls_failed_method.

      l_global_local_class_name = ls_failed_method-class_name.
      l_global_class = mo_metadata_dao->extract_global_class_unit_test( l_global_local_class_name ).
      l_user = mo_metadata_dao->find_class_update_user( l_global_class ).

      send_email_to_user(
        i_user              = l_user
        i_failed_class_name = ls_failed_method-class_name
      ).

    ENDLOOP.

  ENDMETHOD.                    "send_email_for_failures


METHOD send_email_to_user.

    DATA lo_email_sender              TYPE REF TO zcl_cags_send_mail.
    DATA lo_exception                 TYPE REF TO cx_root.
    DATA lt_email_body_with_new_lines TYPE soli_tab.
    DATA l_body_line                  TYPE soli.
    DATA l_empty_line                 TYPE soli.
    DATA lt_recipients                TYPE BCSY_SMTPA.
    DATA ls_recipient                 TYPE ad_smtpadr.
    DATA l_subject                    TYPE string.

    l_empty_line-line = cl_abap_char_utilities=>newline.

    l_body_line-line = |Unit test failed in { sy-sysid }, last transport change found for your SAP user: { i_user }.|.
    APPEND l_body_line TO lt_email_body_with_new_lines.
    l_body_line-line  = |Please check and fix. Class with failing unit test: { i_failed_class_name }.|.
    APPEND l_body_line TO lt_email_body_with_new_lines.
    APPEND l_empty_line TO lt_email_body_with_new_lines..

    l_body_line-line = |See latest tests results:|.
    APPEND l_body_line TO lt_email_body_with_new_lines.
    l_body_line-line = |http://erpdev.statoil.no:6060/view/Unit%20Tests/ |.
    APPEND l_body_line TO lt_email_body_with_new_lines.

    APPEND l_empty_line TO lt_email_body_with_new_lines.
    l_body_line-line = |This is automatically generated message.|.
    APPEND l_body_line TO lt_email_body_with_new_lines.

    l_subject = | Unit test failed in { sy-sysid }, { i_failed_class_name }|.

    TRY.

        ls_recipient = zcl_user_dao=>get_instance( )->find_user_email( i_user ).
        APPEND ls_recipient TO lt_recipients.

        CREATE OBJECT lo_email_sender.

        lo_email_sender->send_email(
          EXPORTING
            p_subject          = l_subject
            p_type             = 'TXT'    " Code for document class
            p_addresses        = lt_recipients
            p_body_tab         = lt_email_body_with_new_lines
            p_priority         = '5'    " Document priority (1 High - 9 Low)
            p_pdf_linsz        = 20
            p_send_immediately = abap_true
        ).


      CATCH cx_root INTO lo_exception.

    ENDTRY.

  ENDMETHOD.                    "send_email_to_user


METHOD set_report_exec_user.

    m_report_exec_user = i_user.

  ENDMETHOD.


METHOD transfer_data_to_hudson.

    DATA i_remote_file_path_string TYPE string.
    i_remote_file_path_string = i_remote_file_path.

    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        filename              = i_remote_file_path_string  "'\\10.216.209.117\ci_workspace'
        filetype              = 'ASC'
*       APPEND                = 'X'
*       write_field_separator = 'X'
*       CONFIRM_OVERWRITE     = 'X'
      TABLES
        data_tab              = it_file_content_lines
      EXCEPTIONS
        file_open_error       = 1
        file_write_error      = 2
        OTHERS                = 3.

  ENDMETHOD.                    "transfer_coverage_to_hudson


METHOD unlock_run.

    DATA l_file_name TYPE string.
    l_file_name = create_sap_file_path( i_file_name = c_file_lock_suffix i_system_id = i_system_id ).
    delete_file( l_file_name ).

  ENDMETHOD.                    "unlock_run


METHOD update_final_coverage_results.

    DATA lo_code_coverage    TYPE REF TO zcl_cags_ci_code_coverage.
    DATA lt_sap_coverage_result TYPE cvt_propro_tkey.
    DATA ls_coverage_result TYPE cvs_propro_tkey.
    DATA l_obj_name TYPE string.
    DATA ls_class_coverage TYPE zcags_ci_code_coverage_s.

    lo_code_coverage = zcl_cags_ci_code_coverage=>get_instance( ).

    IF ( lo_code_coverage->is_coverage_enabled( ) = abap_true ).

      lt_sap_coverage_result = mo_metadata_dao->find_sap_coverage_results( lo_code_coverage->get_coverage_key( ) ).

      LOOP AT lt_sap_coverage_result INTO ls_coverage_result.

        l_obj_name = ls_coverage_result-obj_name.
        IF ( is_customized_name( l_obj_name ) = abap_false ).
          CONTINUE.
        ENDIF.

        CLEAR ls_class_coverage.
        ls_class_coverage = lo_code_coverage->extract_coverage( ls_coverage_result ).
        IF ( ls_class_coverage IS NOT INITIAL ).
          lo_code_coverage->add_class_coverage( is_class_coverage = ls_class_coverage ).
        ENDIF.

      ENDLOOP.

      "lo_code_coverage->save_to_pc_file( ).

    ENDIF.

    ro_coverage_results = lo_code_coverage.

  ENDMETHOD.                    "update_final_coverage_results


METHOD update_final_tests_results.

    DATA lo_unit_tests TYPE REF TO zcl_cags_ci_test_data.
    DATA lt_run_programs TYPE if_saunit_internal_result_type=>ty_t_programs.
    DATA ls_program_info TYPE if_saunit_internal_result_type=>ty_s_program.
    DATA lt_unit_tests_classes TYPE if_saunit_internal_result_type=>ty_t_classes.
    DATA ls_unit_test_class TYPE if_saunit_internal_result_type=>ty_s_class.
    DATA lt_unit_tests_methods TYPE if_saunit_internal_result_type=>ty_t_methods.
    DATA ls_unit_test_method TYPE if_saunit_internal_result_type=>ty_s_method.
    DATA l_unit_test_class_name TYPE string.
    DATA l_unit_test_local_class TYPE string.
    DATA l_unit_test_method TYPE string.
    DATA lt_unit_test_method_alerts TYPE if_saunit_internal_result_type=>ty_t_alerts.
    DATA ls_unit_test_method_alert TYPE if_saunit_internal_result_type=>ty_s_alert.
    DATA l_method_error_message TYPE string.
    DATA l_method_error_msg_line TYPE string.
    DATA lt_method_error_msg_lines TYPE string_table.

    lt_run_programs = it_sap_programs_test_result.

    lo_unit_tests = zcl_cags_ci_test_data=>get_instance( ).

    LOOP AT lt_run_programs INTO ls_program_info.

      l_unit_test_class_name = ls_program_info-info-name.
      lt_unit_tests_classes = ls_program_info-classes.

      LOOP AT lt_unit_tests_classes INTO ls_unit_test_class.
*          l_unit_test_class_name = ls_unit_test_class-info-name.
        l_unit_test_local_class = ls_unit_test_class-info-name.
        lt_unit_tests_methods = ls_unit_test_class-methods.

        LOOP AT lt_unit_tests_methods INTO ls_unit_test_method.

          l_unit_test_method = ls_unit_test_method-info-name. " This is local class test name

* code incompatible with 751
*          IF ( ls_unit_test_method-info-has_error = 'X' ).
*
*            lt_unit_test_method_alerts = ls_unit_test_method-alerts.
*
*            READ TABLE lt_unit_test_method_alerts INDEX 1 INTO ls_unit_test_method_alert.
*            lt_method_error_msg_lines = ls_unit_test_method_alert-header-params.
*            CLEAR l_method_error_message.
*            LOOP AT lt_method_error_msg_lines INTO l_method_error_msg_line.
*              l_method_error_message = l_method_error_message && l_method_error_msg_line.
*            ENDLOOP.
*
*            lo_unit_tests->add_failed_method(
*                i_class_name        = l_unit_test_class_name
*                i_local_test_class  = l_unit_test_local_class
*                i_method_name       = l_unit_test_method
*                i_alert_msg         = l_method_error_message
*            ).
*          ELSE.
*            lo_unit_tests->add_test_method(
*                i_class_name        = l_unit_test_class_name
*                i_local_test_class  = l_unit_test_local_class
*                i_method_name       = l_unit_test_method
*            ).
*          ENDIF.

        ENDLOOP.

      ENDLOOP.

    ENDLOOP.

    ro_test_data = lo_unit_tests.

  ENDMETHOD.                    "update_final_tests_results


METHOD update_summary_save_files.

    DATA lo_coverage TYPE REF TO zcl_cags_ci_code_coverage.
    DATA lo_test_results TYPE REF TO zcl_cags_ci_test_data.
    DATA lt_file_content_package TYPE tchar255.
    DATA lt_file_content_user TYPE tchar255.

    lo_coverage = update_final_coverage_results( ).

    lt_file_content_package = lo_coverage->build_xml_content_package( ).
    save_to_local_sap_dir(
          i_filepath            = create_sap_file_path( ms_report_configuration-coverage_package_file )
          it_file_content_lines = lt_file_content_package
    ).
    transfer_data_to_hudson(
      EXPORTING i_remote_file_path      = create_hudson_file_path( ms_report_configuration-coverage_package_file )
      CHANGING  it_file_content_lines   = lt_file_content_package
    ).

    lt_file_content_user = lo_coverage->build_xml_content_user( ).
    save_to_local_sap_dir(
          i_filepath            = create_sap_file_path( ms_report_configuration-coverage_user_file )
          it_file_content_lines = lt_file_content_user
    ).
    transfer_data_to_hudson(
       EXPORTING  i_remote_file_path     = create_hudson_file_path( ms_report_configuration-coverage_user_file )
       CHANGING   it_file_content_lines  = lt_file_content_user
     ).

    lo_test_results = update_final_tests_results( it_sap_programs_test_result ).

    lo_test_results->build_xml_tests_result(
      IMPORTING
        et_tests_results_package = lt_file_content_package
        et_tests_results_user    = lt_file_content_user ).

    save_to_local_sap_dir(
          i_filepath            = create_sap_file_path( ms_report_configuration-test_package_file )
          it_file_content_lines = lt_file_content_package
    ).
    transfer_data_to_hudson(
      EXPORTING   i_remote_file_path      = create_hudson_file_path( ms_report_configuration-test_package_file )
      CHANGING    it_file_content_lines   = lt_file_content_package
    ).

    save_to_local_sap_dir(
        i_filepath            = create_sap_file_path( ms_report_configuration-test_user_file )
        it_file_content_lines = lt_file_content_user
    ).
    transfer_data_to_hudson(
      EXPORTING   i_remote_file_path      = create_hudson_file_path( ms_report_configuration-test_user_file )
      CHANGING    it_file_content_lines   = lt_file_content_user
    ).

    IF ( ms_report_configuration-email_for_failures = abap_true ).
      send_email_for_failures( lo_test_results ).
    ENDIF.

  ENDMETHOD.                    "update_summary_save_files
ENDCLASS.
