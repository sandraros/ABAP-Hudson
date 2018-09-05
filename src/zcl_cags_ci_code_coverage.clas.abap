*----------------------------------------------------------------------*
*       CLASS ZCL_CAGS_CI_CODE_COVERAGE DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS zcl_cags_ci_code_coverage DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPE-POOLS abap .

    CLASS-METHODS get_instance
      RETURNING
        value(ro_instance) TYPE REF TO zcl_cags_ci_code_coverage .

    CLASS-METHODS set_instance
      IMPORTING
        value(io_instance) TYPE REF TO zcl_cags_ci_code_coverage .

    CLASS-METHODS free_memory .
    METHODS constructor .
    METHODS add_class_coverage
      IMPORTING
        !is_class_coverage TYPE zcags_ci_code_coverage_s .
    METHODS add_coverage_values
      IMPORTING
        !is_new_coverage TYPE zcags_ci_code_coverage_val_s
      CHANGING
        !cs_coverage TYPE zcags_ci_code_coverage_val_s .
    METHODS append_coverage_details
      IMPORTING
        !is_object_code_coverage TYPE zcags_ci_code_coverage_s
      CHANGING
        !ct_xml_content TYPE tchar255 .
    METHODS build_xml_content_package
      RETURNING
        value(rt_code_covare_xml_content) TYPE tchar255 .
    METHODS build_xml_content_user
      RETURNING
        value(rt_code_covare_xml_content) TYPE tchar255 .
    METHODS create_coverage_line
      IMPORTING
        !i_prefix TYPE string
        !i_type TYPE string
        !is_coverage_values TYPE zcags_ci_code_coverage_val_s
      RETURNING
        value(r_coverage_line) TYPE string .
    METHODS create_global_total_coverage
      IMPORTING
        !it_packages_total_coverage TYPE zcags_ci_code_coverage_tt
      RETURNING
        value(rs_global_total_coverage) TYPE zcags_ci_code_coverage_s .
    METHODS create_no_covered_class
      IMPORTING
        !i_object_full_name TYPE string
        !i_object_name TYPE string
        !i_package_name TYPE string
      RETURNING
        value(rs_code_coverage) TYPE zcags_ci_code_coverage_s .
    METHODS create_packages_total_coverage
      RETURNING
        value(rt_packages_total_coverage) TYPE zcags_ci_code_coverage_tt .
    METHODS create_user_total_coverage
      RETURNING
        value(rt_packages_total_coverage) TYPE zcags_ci_code_coverage_tt .
    METHODS extract_coverage
      IMPORTING
        !is_source_coverage TYPE cvs_propro_tkey
      RETURNING
        value(rs_result_coverage) TYPE zcags_ci_code_coverage_s .
    METHODS extract_coverage_from_wunit
      IMPORTING
        !it_source_coverage TYPE cvt_wu
      RETURNING
        value(rs_result_coverage) TYPE zcags_ci_code_coverage_s .
    METHODS get_coverage_key
      RETURNING
        value(r_coverage_key) TYPE saunit_d_coverage_analysis_key .
    METHODS is_coverage_enabled
      RETURNING
        value(r_is_enabled) TYPE abap_bool .
    METHODS read_from_memory .
    METHODS save_to_memory .
    METHODS set_coverage_key
      IMPORTING
        !i_coverage_key TYPE saunit_d_coverage_analysis_key .
    METHODS update_covered_value
      CHANGING
        !cs_coverage_values TYPE zcags_ci_code_coverage_val_s .
    METHODS get_coverage_values
      RETURNING
        value(rt_coverage_result) TYPE zcags_ci_code_coverage_tt .
PROTECTED SECTION.
PRIVATE SECTION.

    CLASS-DATA mo_metadata_dao TYPE REF TO zcl_cags_ci_metadata_dao.

    DATA mt_class_level_coverage TYPE zcags_ci_code_coverage_tt .
    CLASS-DATA mo_instance TYPE REF TO zcl_cags_ci_code_coverage .
    DATA m_coverage_key TYPE saunit_d_coverage_analysis_key .
    CLASS-DATA c_memory_id_class_cov TYPE char35 VALUE 'ZCL_CAGS_CI_CODE_COVERAGE_CLASS_COV'. "#EC NOTEXT .  . " .
    CLASS-DATA c_memory_id_cov_key TYPE c LENGTH 34 VALUE 'ZCL_CAGS_CI_CODE_COVERAGE_COV_KEY'. "#EC NOTEXT .  . " .
ENDCLASS.



CLASS ZCL_CAGS_CI_CODE_COVERAGE IMPLEMENTATION.


METHOD add_class_coverage.
    DATA ls_existing_class_coverage TYPE zcags_ci_code_coverage_s.
    DATA l_row_to_change_index TYPE sy-tabix.

    READ TABLE mt_class_level_coverage INTO ls_existing_class_coverage WITH KEY name = is_class_coverage-name.
    l_row_to_change_index = sy-tabix.

    IF ( ls_existing_class_coverage IS INITIAL ).

      INSERT is_class_coverage INTO TABLE mt_class_level_coverage.

    ELSE.

      ls_existing_class_coverage-class_coverage = is_class_coverage-class_coverage.
      ls_existing_class_coverage-method_coverage = is_class_coverage-method_coverage.
      ls_existing_class_coverage-block_coverage = is_class_coverage-block_coverage.
      ls_existing_class_coverage-line_coverage = is_class_coverage-line_coverage.

      MODIFY mt_class_level_coverage INDEX l_row_to_change_index FROM ls_existing_class_coverage.

    ENDIF.

  ENDMETHOD.                    "add_class_coverage


METHOD add_coverage_values.

    cs_coverage-count_covered = cs_coverage-count_covered + is_new_coverage-count_covered.
    cs_coverage-count_total = cs_coverage-count_total + is_new_coverage-count_total.
    cs_coverage-percent = cs_coverage-count_covered * 100 / cs_coverage-count_total.

  ENDMETHOD.                    "ADD_COVERAGE_VALUES


METHOD append_coverage_details.

    DATA l_line TYPE string.

    l_line = create_coverage_line( i_prefix = '      ' i_type = 'class' is_coverage_values = is_object_code_coverage-class_coverage ).
    APPEND l_line TO ct_xml_content.
    l_line = create_coverage_line( i_prefix = '      ' i_type = 'method' is_coverage_values = is_object_code_coverage-method_coverage ).
    APPEND l_line TO ct_xml_content.
    l_line = create_coverage_line( i_prefix = '      ' i_type = 'block' is_coverage_values = is_object_code_coverage-block_coverage ).
    APPEND l_line TO ct_xml_content.
    l_line = create_coverage_line( i_prefix = '      ' i_type = 'line' is_coverage_values = is_object_code_coverage-line_coverage ).
    APPEND l_line TO ct_xml_content.


  ENDMETHOD.                    "append_coverage_details


METHOD build_xml_content_package.

    DATA lt_code_coverage_xml_content TYPE tchar255.
    DATA l_datetime TYPE string.
    l_datetime = |{ sy-datum } { sy-uzeit }|.

    DATA l_line TYPE string.
    l_line = |<?xml version="1.0" encoding="UTF-8"?><!-- EMMA report from ABAP -->|.
    APPEND l_line TO lt_code_coverage_xml_content.
    l_line = |<report created="{ l_datetime }">|.
    APPEND l_line TO lt_code_coverage_xml_content.
    l_line = |  <data>|.
    APPEND l_line TO lt_code_coverage_xml_content.
    l_line = |    <all name="all classes">|.
    APPEND l_line TO lt_code_coverage_xml_content.


    DATA ls_package_coverage TYPE zcags_ci_code_coverage_s.
    DATA l_package_name TYPE zcags_ci_code_coverage_s-package.
    DATA l_package_full_path TYPE string.

    DATA ls_class_coverage  TYPE zcags_ci_code_coverage_s.

    DATA ls_global_total_coverage TYPE zcags_ci_code_coverage_s.

    DATA ls_package_total_coverage TYPE zcags_ci_code_coverage_s.
    DATA lt_package_total_coverage TYPE zcags_ci_code_coverage_tt.

    lt_package_total_coverage = create_packages_total_coverage( ).

    ls_global_total_coverage = create_global_total_coverage( lt_package_total_coverage ).

    append_coverage_details(
      EXPORTING
        is_object_code_coverage =     ls_global_total_coverage
      CHANGING
        ct_xml_content          =     lt_code_coverage_xml_content
    ).

    " For user names packages
    DATA l_displayed_package_name TYPE string.
    DATA l_is_test_for_user TYPE abap_bool.

    LOOP AT lt_package_total_coverage INTO ls_package_coverage.

      l_package_name = ls_package_coverage-parent_object.

      l_package_full_path =  mo_metadata_dao->find_full_package_path( l_package_name ).
      l_displayed_package_name = l_package_full_path.

      l_line = |      <package name="{ l_displayed_package_name }">|.
      APPEND l_line TO lt_code_coverage_xml_content.

      append_coverage_details(
        EXPORTING
          is_object_code_coverage =     ls_package_coverage
        CHANGING
          ct_xml_content          =     lt_code_coverage_xml_content
      ).

      LOOP AT mt_class_level_coverage INTO ls_class_coverage WHERE parent_object = l_package_name.

        l_line = |        <srcfile name="{ ls_class_coverage-name }">|.
        APPEND l_line TO lt_code_coverage_xml_content.

        append_coverage_details(
          EXPORTING
            is_object_code_coverage =     ls_class_coverage
          CHANGING
            ct_xml_content          =     lt_code_coverage_xml_content
        ).

        l_line = |          <class name="{ ls_class_coverage-name }">|.
        APPEND l_line TO lt_code_coverage_xml_content.

        append_coverage_details(
          EXPORTING
            is_object_code_coverage =     ls_class_coverage
          CHANGING
            ct_xml_content          =     lt_code_coverage_xml_content
        ).

        l_line = |      </class>|.
        APPEND l_line TO lt_code_coverage_xml_content.
        l_line = |      </srcfile>|.
        APPEND l_line TO lt_code_coverage_xml_content.

      ENDLOOP.

      l_line = |      </package>|.
      APPEND l_line TO lt_code_coverage_xml_content.

    ENDLOOP.

    l_line = |    </all>|.
    APPEND l_line TO lt_code_coverage_xml_content.
    l_line = |  </data>|.
    APPEND l_line TO lt_code_coverage_xml_content.
    l_line = |</report>|.
    APPEND l_line TO lt_code_coverage_xml_content.

    rt_code_covare_xml_content = lt_code_coverage_xml_content.

  ENDMETHOD.                    "build_xml_content_package


METHOD build_xml_content_user.

    DATA lt_code_coverage_xml_content TYPE tchar255.
    DATA l_datetime TYPE string.
    l_datetime = |{ sy-datum } { sy-uzeit }|.

    DATA l_line TYPE string.
    l_line = |<?xml version="1.0" encoding="UTF-8"?><!-- EMMA report from ABAP -->|.
    APPEND l_line TO lt_code_coverage_xml_content.
    l_line = |<report created="{ l_datetime }">|.
    APPEND l_line TO lt_code_coverage_xml_content.
    l_line = |  <data>|.
    APPEND l_line TO lt_code_coverage_xml_content.
    l_line = |    <all name="all classes">|.
    APPEND l_line TO lt_code_coverage_xml_content.


    DATA ls_package_coverage TYPE zcags_ci_code_coverage_s.
    DATA l_package_name TYPE string.
    DATA l_package_full_path TYPE string.

    DATA ls_class_coverage  TYPE zcags_ci_code_coverage_s.

    DATA ls_global_total_coverage TYPE zcags_ci_code_coverage_s.

    DATA ls_package_total_coverage TYPE zcags_ci_code_coverage_s.
    DATA lt_package_total_coverage TYPE zcags_ci_code_coverage_tt.

    lt_package_total_coverage = create_user_total_coverage( ).

    ls_global_total_coverage = create_global_total_coverage( lt_package_total_coverage ).

    append_coverage_details(
      EXPORTING
        is_object_code_coverage =     ls_global_total_coverage
      CHANGING
        ct_xml_content          =     lt_code_coverage_xml_content
    ).

    " For user names packages
    DATA l_displayed_package_name TYPE string.
    DATA l_is_test_for_user TYPE abap_bool.

    LOOP AT lt_package_total_coverage INTO ls_package_coverage.

      l_package_name = ls_package_coverage-parent_object.

      l_displayed_package_name = ls_package_coverage-name.  " package represented as user name

      l_line = |      <package name="{ l_displayed_package_name }">|.
      APPEND l_line TO lt_code_coverage_xml_content.

      append_coverage_details(
        EXPORTING
          is_object_code_coverage =     ls_package_coverage
        CHANGING
          ct_xml_content          =     lt_code_coverage_xml_content
      ).

      LOOP AT mt_class_level_coverage INTO ls_class_coverage WHERE user = ls_package_coverage-user.

        l_line = |        <srcfile name="{ ls_class_coverage-name }">|.
        APPEND l_line TO lt_code_coverage_xml_content.

        append_coverage_details(
          EXPORTING
            is_object_code_coverage =     ls_class_coverage
          CHANGING
            ct_xml_content          =     lt_code_coverage_xml_content
        ).

        l_line = |          <class name="{ ls_class_coverage-name }">|.
        APPEND l_line TO lt_code_coverage_xml_content.

        append_coverage_details(
          EXPORTING
            is_object_code_coverage =     ls_class_coverage
          CHANGING
            ct_xml_content          =     lt_code_coverage_xml_content
        ).

        l_line = |      </class>|.
        APPEND l_line TO lt_code_coverage_xml_content.
        l_line = |      </srcfile>|.
        APPEND l_line TO lt_code_coverage_xml_content.

      ENDLOOP.

      l_line = |      </package>|.
      APPEND l_line TO lt_code_coverage_xml_content.

    ENDLOOP.

    l_line = |    </all>|.
    APPEND l_line TO lt_code_coverage_xml_content.
    l_line = |  </data>|.
    APPEND l_line TO lt_code_coverage_xml_content.
    l_line = |</report>|.
    APPEND l_line TO lt_code_coverage_xml_content.

    rt_code_covare_xml_content = lt_code_coverage_xml_content.

  ENDMETHOD.                    "BUILD_XML_CONTENT_USER


METHOD constructor.

    mo_metadata_dao = zcl_cags_ci_metadata_dao=>get_instance( ).

  ENDMETHOD.                    "constructor


METHOD create_coverage_line.

    r_coverage_line = |        <coverage type="{ i_type }, %" value="{ is_coverage_values-percent }% ({ is_coverage_values-count_covered }/{ is_coverage_values-count_total })"/>|.

  ENDMETHOD.                    "create_coverage_line


METHOD create_global_total_coverage.


    DATA ls_global_total_coverage TYPE zcags_ci_code_coverage_s.
    DATA ls_package_coverage TYPE zcags_ci_code_coverage_s.
    DATA l_package_name TYPE string.



    LOOP AT it_packages_total_coverage INTO ls_package_coverage.

      add_coverage_values(
        EXPORTING is_new_coverage = ls_package_coverage-class_coverage
        CHANGING  cs_coverage     = ls_global_total_coverage-class_coverage
      ).

      add_coverage_values(
        EXPORTING is_new_coverage = ls_package_coverage-method_coverage
        CHANGING  cs_coverage     = ls_global_total_coverage-method_coverage
      ).

      add_coverage_values(
        EXPORTING is_new_coverage = ls_package_coverage-block_coverage
        CHANGING  cs_coverage     = ls_global_total_coverage-block_coverage
      ).

      add_coverage_values(
        EXPORTING is_new_coverage = ls_package_coverage-line_coverage
        CHANGING  cs_coverage     = ls_global_total_coverage-line_coverage
      ).

    ENDLOOP.

    rs_global_total_coverage = ls_global_total_coverage.

  ENDMETHOD.                    "CREATE_GLOBAL_TOTAL_COVERAGE


METHOD create_no_covered_class.

    DATA ls_class_coverage TYPE zcags_ci_code_coverage_s.
    DATA l_is_class_deleted TYPE abap_bool.
    DATA ls_coveraga_empty_select TYPE cvs_select.
    DATA lt_coverage_results TYPE cvt_wu.
    DATA ls_coverage_line TYPE cvs_wu.
    DATA l_program_name TYPE program.
    DATA ls_coverage_values TYPE zcags_ci_code_coverage_s.
    ls_class_coverage-name = i_object_name.
    ls_class_coverage-parent_object = i_package_name.
    ls_class_coverage-class_coverage-count_covered = 0.
    ls_class_coverage-class_coverage-count_total = 1.
    ls_class_coverage-class_coverage-percent = 0.

    mo_metadata_dao->find_class_metadata(
      EXPORTING
        i_class_name =     ls_class_coverage-name
        i_package    =     ls_class_coverage-parent_object
      IMPORTING
        e_user       =     ls_class_coverage-user
        e_is_deleted =     l_is_class_deleted
    ).

    IF ( l_is_class_deleted = abap_true ).

      CLEAR rs_code_coverage.

    ELSE.

      l_program_name = i_object_full_name.

      TRY.
          cl_coverage_eval=>build_wunit(
            EXPORTING
              im_progname =     l_program_name
              im_select   =     ls_coveraga_empty_select
            IMPORTING
              ex_wunit    =     lt_coverage_results
          ).

          ls_coverage_values = extract_coverage_from_wunit( lt_coverage_results ).
          ls_class_coverage-class_coverage = ls_coverage_values-class_coverage.
          ls_class_coverage-method_coverage = ls_coverage_values-method_coverage.
          ls_class_coverage-block_coverage = ls_coverage_values-block_coverage.
          ls_class_coverage-line_coverage = ls_coverage_values-line_coverage.

        CATCH cx_coverage_root.    " In case of error just create not covered class with 0 statistics
          ls_class_coverage-class_coverage-percent = 0.
          ls_class_coverage-class_coverage-count_covered = 0.
          ls_class_coverage-class_coverage-count_total = 1.
      ENDTRY.

      rs_code_coverage = ls_class_coverage.

    ENDIF.


  ENDMETHOD.                    "create_no_covered_class


METHOD create_packages_total_coverage.

    DATA lt_package_total_coverage TYPE zcags_ci_code_coverage_tt.
    DATA ls_package_total_coverage TYPE zcags_ci_code_coverage_s.
    DATA ls_package_coverage TYPE zcags_ci_code_coverage_s.
    DATA l_package_name TYPE string.

    LOOP AT mt_class_level_coverage INTO ls_package_coverage.
      l_package_name = ls_package_coverage-parent_object.

      AT NEW parent_object.
        CLEAR ls_package_total_coverage.
        ls_package_total_coverage-parent_object = l_package_name.
      ENDAT.

      add_coverage_values(
        EXPORTING is_new_coverage = ls_package_coverage-class_coverage
        CHANGING  cs_coverage     = ls_package_total_coverage-class_coverage
      ).

      add_coverage_values(
        EXPORTING is_new_coverage = ls_package_coverage-method_coverage
        CHANGING  cs_coverage     = ls_package_total_coverage-method_coverage
      ).

      add_coverage_values(
        EXPORTING is_new_coverage = ls_package_coverage-block_coverage
        CHANGING  cs_coverage     = ls_package_total_coverage-block_coverage
      ).

      add_coverage_values(
        EXPORTING is_new_coverage = ls_package_coverage-line_coverage
        CHANGING  cs_coverage     = ls_package_total_coverage-line_coverage
      ).

      AT END OF parent_object.
        INSERT ls_package_total_coverage INTO TABLE lt_package_total_coverage.
      ENDAT.

    ENDLOOP.

    rt_packages_total_coverage = lt_package_total_coverage.

  ENDMETHOD.                    "CREATE_PACKAGES_TOTAL_COVERAGE


METHOD create_user_total_coverage.

    DATA lt_package_total_coverage TYPE zcags_ci_code_coverage_tt.
    DATA ls_package_total_coverage TYPE zcags_ci_code_coverage_s.
    DATA ls_class_coverage TYPE zcags_ci_code_coverage_s.
    DATA l_package_name TYPE string.

    DATA l_user TYPE uname.
    DATA l_index_to_update TYPE sy-tabix.

    DATA l_class_name TYPE string.

    DATA ls_current_class_coverage TYPE zcags_ci_code_coverage_s.

    LOOP AT mt_class_level_coverage INTO ls_class_coverage.
      l_class_name = ls_class_coverage-name.

      l_user = ls_class_coverage-user.
      IF ( l_user IS INITIAL ).
        l_user = mo_metadata_dao->find_class_user( l_class_name ).
      ENDIF.

      READ TABLE lt_package_total_coverage WITH KEY name = l_user INTO ls_current_class_coverage.

      IF ( sy-subrc <> 0 ).
        " If does not exists then prepare new object
        ls_class_coverage-name = l_user.
        INSERT ls_class_coverage INTO TABLE lt_package_total_coverage.

      ELSE.

        l_index_to_update = sy-tabix.

        add_coverage_values(
         EXPORTING is_new_coverage = ls_class_coverage-class_coverage
         CHANGING  cs_coverage     = ls_current_class_coverage-class_coverage
       ).

        add_coverage_values(
          EXPORTING is_new_coverage = ls_class_coverage-method_coverage
          CHANGING  cs_coverage     = ls_current_class_coverage-method_coverage
        ).

        add_coverage_values(
          EXPORTING is_new_coverage = ls_class_coverage-block_coverage
          CHANGING  cs_coverage     = ls_current_class_coverage-block_coverage
        ).

        add_coverage_values(
          EXPORTING is_new_coverage = ls_class_coverage-line_coverage
          CHANGING  cs_coverage     = ls_current_class_coverage-line_coverage
        ).

        MODIFY lt_package_total_coverage FROM ls_current_class_coverage INDEX l_index_to_update.

      ENDIF.


    ENDLOOP.

    rt_packages_total_coverage = lt_package_total_coverage.

  ENDMETHOD.                    "create_user_total_coverage


METHOD extract_coverage.

    DATA l_package_full_path TYPE string.
    DATA l_package_name TYPE string.
    DATA l_is_class_deleted TYPE abap_bool.

    CLEAR rs_result_coverage.

    l_package_name = is_source_coverage-devclass.
    rs_result_coverage-parent_object = l_package_name.
    rs_result_coverage-name = is_source_coverage-obj_name.

    mo_metadata_dao->find_class_metadata(
      EXPORTING
        i_class_name    = rs_result_coverage-name
        i_package       = rs_result_coverage-parent_object
      IMPORTING
        e_user        = rs_result_coverage-user
        e_is_deleted  = l_is_class_deleted
    ).

    IF ( l_is_class_deleted = abap_true ).

      CLEAR rs_result_coverage.

    ELSE.

      rs_result_coverage-class_coverage-percent = 100.
      rs_result_coverage-class_coverage-count_total = 1.
      update_covered_value( CHANGING cs_coverage_values = rs_result_coverage-class_coverage ).

      rs_result_coverage-method_coverage-percent = is_source_coverage-proz_cnt_cum.
      rs_result_coverage-method_coverage-count_total = is_source_coverage-total.
      update_covered_value( CHANGING cs_coverage_values = rs_result_coverage-method_coverage ).

      rs_result_coverage-block_coverage-percent = is_source_coverage-proz_cnt_cum_brch.
      rs_result_coverage-block_coverage-count_total = is_source_coverage-branch_count.
      update_covered_value( CHANGING cs_coverage_values = rs_result_coverage-block_coverage ).

      rs_result_coverage-line_coverage-percent = is_source_coverage-proz_cnt_cum_st.
      rs_result_coverage-line_coverage-count_total = is_source_coverage-statement_count.
      update_covered_value( CHANGING cs_coverage_values = rs_result_coverage-line_coverage ).

    ENDIF.


  ENDMETHOD.                    "extract_coverage


METHOD extract_coverage_from_wunit.

    DATA l_methods_count TYPE zcags_ci_code_coverage_val_s-count_total.
    DATA l_blocks_count TYPE zcags_ci_code_coverage_val_s-count_total.
    DATA l_statements_count TYPE zcags_ci_code_coverage_val_s-count_total.
    FIELD-SYMBOLS <ls_source_coverage_details> TYPE cvs_wu.

    CLEAR rs_result_coverage.

    LOOP AT it_source_coverage ASSIGNING <ls_source_coverage_details>.
      l_methods_count = l_methods_count + 1.
      l_blocks_count = l_blocks_count + <ls_source_coverage_details>-branch_count.
      l_statements_count = l_statements_count + <ls_source_coverage_details>-statement_count.
    ENDLOOP.

    rs_result_coverage-class_coverage-percent = 0.
    rs_result_coverage-class_coverage-count_total = 1.
    rs_result_coverage-class_coverage-count_covered = 0.

    rs_result_coverage-method_coverage-percent = 0.
    rs_result_coverage-method_coverage-count_covered = 0.
    rs_result_coverage-method_coverage-count_total = l_methods_count.

    rs_result_coverage-block_coverage-percent = 0.
    rs_result_coverage-block_coverage-count_covered = 0.
    rs_result_coverage-block_coverage-count_total = l_blocks_count.

    rs_result_coverage-line_coverage-percent = 0.
    rs_result_coverage-line_coverage-count_covered = 0.
    rs_result_coverage-line_coverage-count_total = l_statements_count.

  ENDMETHOD.                    "extract_coverage_from_wunit


METHOD free_memory.

    FREE MEMORY ID c_memory_id_class_cov.
    FREE MEMORY ID c_memory_id_cov_key.

  ENDMETHOD.                    "FREE_MEMORY


METHOD get_coverage_key.

    r_coverage_key = m_coverage_key.

  ENDMETHOD.                    "GET_COVERAGE_KEY


METHOD get_coverage_values.

    rt_coverage_result = mt_class_level_coverage.

  ENDMETHOD.                    "get_coverage_values


METHOD get_instance.

    IF ( mo_instance IS INITIAL ).
      CREATE OBJECT mo_instance.
      mo_instance->read_from_memory( ).
    ENDIF.

    ro_instance = mo_instance.

  ENDMETHOD.                    "GET_INSTANCE


METHOD is_coverage_enabled.

    r_is_enabled = abap_true."zcl_cags_ci_report=>get_instance( )->is_on( ).

  ENDMETHOD.                    "IS_COVERAGE_ENABLED


METHOD read_from_memory.

    IMPORT class_coverage_table TO mt_class_level_coverage FROM MEMORY ID c_memory_id_class_cov.
    IMPORT coverage_key TO m_coverage_key FROM MEMORY ID c_memory_id_cov_key.

  ENDMETHOD.                    "READ_FROM_MEMORY


METHOD save_to_memory.

    EXPORT class_coverage_table FROM mt_class_level_coverage TO MEMORY ID c_memory_id_class_cov.
    EXPORT coverage_key FROM m_coverage_key TO MEMORY ID c_memory_id_cov_key.

  ENDMETHOD.                    "save_to_memory


METHOD set_coverage_key.

    m_coverage_key = i_coverage_key.

  ENDMETHOD.                    "SET_COVERAGE_KEY


METHOD set_instance.

    mo_instance = io_instance.

  ENDMETHOD.                    "GET_INSTANCE


METHOD update_covered_value.

    cs_coverage_values-count_covered =
        cs_coverage_values-count_total * cs_coverage_values-percent / 100.

  ENDMETHOD.                    "UPDATE_COVERED_VALUE
ENDCLASS.
