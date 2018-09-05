FUNCTION ZCAGS_CI_RUN_TESTS_BATCH.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_EXEC_USER) TYPE  SY-UNAME DEFAULT SY-UNAME
*"  EXPORTING
*"     VALUE(E_RESULT_LOG) TYPE  STRING
*"----------------------------------------------------------------------
DATA lo_caught_exc TYPE REF TO zcx_cags_ci_report_exception.
  DATA lo_ci_report TYPE REF TO zcl_cags_ci_report.

  lo_ci_report = zcl_cags_ci_report=>get_instance( ).
  lo_ci_report->set_report_exec_user( i_exec_user ).
  lo_ci_report->init_default_files_path( ).

  TRY.
      IF ( lo_ci_report->is_run_lock_file( ) = abap_true ).

        zcx_cags_ci_report_exception=>raise_exception( 'ERROR: Cannot start new test. Unit Test report is already in progress...' ).

      ELSE.

        lo_ci_report->run_unit_tests_default_variant( ).
        e_result_log = 'Testing job scheduled'.

      ENDIF.

    CATCH zcx_cags_ci_report_exception INTO lo_caught_exc.

      e_result_log = lo_caught_exc->get_msg_with_code( ).

  ENDTRY.





ENDFUNCTION.
