*----------------------------------------------------------------------*
*       CLASS zcl_cags_ci_http_req_handler DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS zcl_cags_ci_http_req_handler DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_resource_handler .

    METHODS action_read_file
     IMPORTING
       i_file_path TYPE file_name
     RETURNING
       value(r_file_content) TYPE string.
PROTECTED SECTION.
PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_CAGS_CI_HTTP_REQ_HANDLER IMPLEMENTATION.


METHOD action_read_file.

    DATA l_response TYPE string.
    DATA l_xml_content_line TYPE char255.

    OPEN DATASET i_file_path FOR INPUT IN TEXT MODE ENCODING DEFAULT WITH WINDOWS LINEFEED.

    IF ( sy-subrc <> 0 ).
      zcx_cags_ci_report_exception=>raise_exception(
        i_code = sy-subrc
        i_msg =  'ERROR: Cannot open file for response:' && i_file_path
      ).
    ENDIF.

    DO.

      READ DATASET i_file_path INTO l_xml_content_line.

      l_response = l_response && l_xml_content_line.

      IF ( sy-subrc <> 0 ).
        EXIT.
      ENDIF.

    ENDDO.

    CLOSE DATASET i_file_path.

    r_file_content = l_response.

  ENDMETHOD.                    "action_get_file


METHOD zif_resource_handler~handle_delete.

  ENDMETHOD.                    "zif_resource_handler~handle_delete


METHOD zif_resource_handler~handle_get.
    DATA l_param1       LIKE LINE OF it_params.
    DATA l_http_command TYPE string.
    DATA l_file_name    TYPE file_name.
    DATA lo_caught_exc  TYPE REF TO zcx_cags_ci_report_exception.
    DATA lo_ci_report   TYPE REF TO zcl_cags_ci_report.
    DATA l_command      TYPE string.
    DATA l_system       TYPE sy-sysid.
    DATA l_user         TYPE sy-uname.


    READ TABLE it_params INDEX 1 INTO l_param1.
    l_http_command = l_param1-value.

    TRY.
        lo_ci_report = zcl_cags_ci_report=>get_instance( ).
        lo_ci_report->init_default_files_path( ).

        SPLIT l_http_command AT '_' INTO l_command l_system l_user.
        IF ( l_user IS INITIAL ).
          l_user = sy-uname.
        ENDIF.
        IF ( l_system IS INITIAL ).
          l_system = sy-sysid.
        ENDIF.

        CASE l_command.

          WHEN 'JOBRUN' .

            IF ( l_system = 'D07' ).
              CALL FUNCTION 'ZCAGS_CI_RUN_TESTS_BATCH' DESTINATION 'D07CLI260'
                EXPORTING
                  i_exec_user  = l_user
                IMPORTING
                  e_result_log = ev_content. " 751
*                  e_result_log = result-content.

            ELSEIF l_system = 'D87'.

              CALL FUNCTION 'ZCAGS_CI_RUN_TESTS_BATCH' DESTINATION 'D87CLI275'
                EXPORTING
                  i_exec_user  = l_user
                IMPORTING
                  e_result_log = ev_content. " 751
*                  e_result_log = result-content.

            ELSE.

              CALL FUNCTION 'ZCAGS_CI_RUN_TESTS_BATCH'
                EXPORTING
                  i_exec_user  = l_user
                IMPORTING
                  e_result_log = ev_content. " 751

            ENDIF.

          WHEN 'DELLOCK'.

            lo_ci_report->unlock_run( l_system ).
            ev_content = 'Run unlocked'. " 751
*            result-content = 'Run unlocked'.

          WHEN 'DELFILES'.

            lo_ci_report->delete_sap_files( l_system ).
            ev_content = 'Files deleted'. " 751
*            result-content = 'Files deleted'.

          WHEN OTHERS.

            l_file_name = l_http_command.
            l_file_name = lo_ci_report->get_sap_dir( ) && l_file_name.
            ev_content = action_read_file( l_file_name ).
*            result-content = action_read_file( l_file_name ).

        ENDCASE.

        ev_status_code = 0.
*        result-statuscode = 0.

      CATCH zcx_cags_ci_report_exception INTO lo_caught_exc.

        ev_content = lo_caught_exc->get_msg_with_code( ).
        ev_status_code = 0.
*        result-content = lo_caught_exc->get_msg_with_code( ).
*        result-statuscode = 0.

      CATCH cx_root.

        ev_content = 'CX_ROOT caught'.
        ev_status_code = 1.
*        result-content = 'CX_ROOT caught'.
*        result-statuscode = 1.

    ENDTRY.


  ENDMETHOD.                    "zif_resource_handler~handle_get


METHOD zif_resource_handler~handle_post.

  ENDMETHOD.                    "zif_resource_handler~handle_post


METHOD zif_resource_handler~handle_put.

  ENDMETHOD.                    "zif_resource_handler~handle_put
ENDCLASS.
