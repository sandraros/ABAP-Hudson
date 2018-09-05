*----------------------------------------------------------------------*
*       CLASS ZCL_CAGS_CI_METADATA_DAO DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS zcl_cags_ci_metadata_dao DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor.

    CLASS-METHODS get_instance
    RETURNING value(ro_instance) TYPE REF TO zcl_cags_ci_metadata_dao.

    METHODS find_class_user
      IMPORTING
        !i_class_name TYPE string
      RETURNING
        value(r_class_author_user) TYPE uname .
    METHODS find_class_package_full_path
      IMPORTING
        !i_class_name TYPE string
      RETURNING
        value(r_class_package) TYPE string .
    METHODS find_full_package_path
      IMPORTING
        !i_package_name TYPE zcags_ci_code_coverage_s-package
      RETURNING
        value(r_package_full_path) TYPE string .
    TYPE-POOLS abap .
    METHODS find_class_metadata
      IMPORTING
        !i_class_name TYPE zcags_ci_code_coverage_s-name
        !i_package TYPE zcags_ci_code_coverage_s-parent_object
      EXPORTING
        !e_user TYPE zcags_ci_code_coverage_s-user
        !e_is_deleted TYPE abap_bool .
    METHODS find_custom_root_packages
      RETURNING
        value(rt_root_packages) TYPE tr_devclasses .

    METHODS find_sap_coverage_results
      IMPORTING
        i_coverage_key TYPE saunit_d_coverage_analysis_key
      RETURNING
        value(rt_sap_coverage_result) TYPE cvt_propro_tkey.

    METHODS find_full_package_path_child
        IMPORTING
          !i_package_name TYPE zcags_ci_code_coverage_s-package
        RETURNING
          value(r_package_full_path) TYPE string .

    CLASS-METHODS set_instance
      IMPORTING
        io_dao_instance TYPE REF TO zcl_cags_ci_metadata_dao.


    METHODS find_class_update_user
      IMPORTING
        i_class_name TYPE string
      RETURNING
        value(r_last_update_class_user) TYPE sy-uname.

    METHODS extract_global_class_unit_test
      IMPORTING
        i_class_with_unit_test_name TYPE string
      RETURNING
        value(r_global_class_name) TYPE string.
PROTECTED SECTION.
PRIVATE SECTION.

    CLASS-DATA mo_instance TYPE REF TO zcl_cags_ci_metadata_dao.

    TYPES mt_char TYPE TABLE OF ausp.
    DATA lt_my_table TYPE mt_char.

*    TYPES  lt_equip_func_loc TYPE SORTED TABLE OF equz WITH NON-UNIQUE KEY equnr aedat. " 751
ENDCLASS.



CLASS ZCL_CAGS_CI_METADATA_DAO IMPLEMENTATION.


METHOD constructor.

  ENDMETHOD.                    "constructor


METHOD extract_global_class_unit_test.

    DATA l_global_class_name TYPE string.
    DATA l_local_class_name TYPE string.
    SPLIT i_class_with_unit_test_name AT '~' INTO l_global_class_name l_local_class_name.
    r_global_class_name = l_global_class_name.

  ENDMETHOD.                    "extract_global_class_unit_test


METHOD find_class_metadata.


    DATA l_class_name TYPE seoclass-clsname.
    DATA ls_class_details TYPE tadir.
    l_class_name = i_class_name.

    SELECT SINGLE * FROM tadir INTO ls_class_details WHERE devclass = i_package AND obj_name = i_class_name.

    e_user = ls_class_details-author.

    IF e_user IS INITIAL.
      e_user = 'UNKNOWN'.
    ENDIF.

    e_is_deleted = ls_class_details-delflag.

  ENDMETHOD.                    "FIND_CLASS_METADATA


METHOD find_class_package_full_path.

    DATA ls_class_details TYPE tadir.
    DATA l_package_name TYPE zcags_ci_code_coverage_s-package.
    DATA l_full_package_path TYPE string.

    SELECT SINGLE * FROM tadir INTO ls_class_details WHERE obj_name = i_class_name.

    IF ( sy-subrc = 0 ).
      l_package_name = ls_class_details-devclass.
      l_full_package_path = find_full_package_path( l_package_name ).
      r_class_package = l_full_package_path.
    ELSE.
      CLEAR r_class_package.
    ENDIF.

    IF ( r_class_package IS INITIAL ).
      r_class_package = 'PACKAGE_UNKNOWN'.
    ENDIF.

  ENDMETHOD.                    "FIND_CLASS_PACKAGE_FULL_PATH


METHOD find_class_update_user.

    DATA ls_user_change_data TYPE reposrc.
    DATA l_program_name_parttern TYPE string.
    DATA l_program_cp_to_ignore TYPE reposrc-progname.

    " Find exact name if program and not class
    SELECT * FROM reposrc
        INTO ls_user_change_data
          WHERE progname = i_class_name AND r3state = 'A'
            ORDER BY udat DESCENDING utime DESCENDING.
      r_last_update_class_user = ls_user_change_data-unam.
      EXIT.
    ENDSELECT.

    IF ( r_last_update_class_user IS INITIAL ).
      " use class pattern if exact name not found

      l_program_name_parttern = i_class_name.

      WHILE strlen( l_program_name_parttern ) < 30.
        CONCATENATE  l_program_name_parttern '=' INTO l_program_name_parttern.
      ENDWHILE.

      l_program_cp_to_ignore = l_program_name_parttern && 'CP'.
      l_program_name_parttern = l_program_name_parttern && '%'.

      SELECT * FROM reposrc "progdir
        INTO ls_user_change_data
          WHERE progname LIKE l_program_name_parttern AND r3state = 'A'
            ORDER BY udat DESCENDING utime DESCENDING.
        IF ( ls_user_change_data-progname <> l_program_cp_to_ignore ).
          r_last_update_class_user = ls_user_change_data-unam.
          EXIT.
        ENDIF.
      ENDSELECT.
    ENDIF.

  ENDMETHOD.                    "find_class_update_user


METHOD find_class_user.

    DATA l_class_name TYPE seoclass-clsname.
    DATA ls_class_details TYPE tadir.

    l_class_name = i_class_name.

    SELECT SINGLE * FROM tadir INTO ls_class_details WHERE obj_name = i_class_name.

    r_class_author_user = ls_class_details-author.

    IF r_class_author_user IS INITIAL.
      r_class_author_user = 'UNKNOWN'.
    ENDIF.

  ENDMETHOD.                    "FIND_CLASS_USER


METHOD find_custom_root_packages.

    DATA lt_root_packages TYPE tr_devclasses.
    SELECT devclass FROM tdevc INTO TABLE lt_root_packages WHERE parentcl = '' AND ( devclass LIKE 'Z%' OR devclass LIKE 'Y%' ).

    rt_root_packages = lt_root_packages.

  ENDMETHOD.                    "FIND_CUSTOM_ROOT_PACKAGES


METHOD find_full_package_path.

    DATA l_parent_package TYPE string.
    DATA l_package_full_path TYPE string.
    DATA l_parent_package_may_exist TYPE abap_bool VALUE abap_true.
    l_package_full_path = i_package_name.

    l_parent_package = i_package_name.

    WHILE ( l_parent_package_may_exist = abap_true ).

      SELECT SINGLE parentcl FROM tdevc INTO l_parent_package WHERE devclass = l_parent_package.

      IF ( sy-subrc <> 0 OR l_parent_package IS INITIAL ).

        l_parent_package_may_exist = abap_false.

      ELSE.

        l_package_full_path = l_parent_package && '.' && l_package_full_path.

      ENDIF.

    ENDWHILE.

    r_package_full_path = l_package_full_path.

  ENDMETHOD.                    "FIND_FULL_PACKAGE_PATH


METHOD find_full_package_path_child.

    DATA l_child_package TYPE string.
    DATA l_last_package TYPE string.
    DATA l_package_full_path TYPE string.
    DATA l_child_package_may_exist TYPE abap_bool VALUE abap_true.
    l_package_full_path = i_package_name.

    l_last_package = i_package_name.

    WHILE ( l_child_package_may_exist = abap_true ).
      l_child_package_may_exist = abap_false.
      "SELECT SINGLE parentcl FROM tdevc INTO l_child_package WHERE devclass = l_child_package.
      SELECT devclass FROM tdevc INTO l_child_package WHERE parentcl = l_last_package.

        IF ( sy-subrc <> 0 OR l_child_package IS NOT INITIAL ).

          l_child_package_may_exist = abap_true.

        ELSE.

          "l_package_full_path = l_child_package && '.' && l_package_full_path.
          l_package_full_path = l_package_full_path && '.' && l_child_package.

        ENDIF.

      ENDSELECT.

    ENDWHILE.

    r_package_full_path = l_package_full_path.

  ENDMETHOD.                    "FIND_FULL_PACKAGE_PATH


METHOD find_sap_coverage_results.

    DATA:
      lt_devc_ranges        TYPE cvt_sodev,
      lt_devc_range         LIKE LINE OF lt_devc_ranges,
      lt_prog_ranges        TYPE cvt_sopro,
      lt_prog_range         LIKE LINE OF lt_prog_ranges,
      lt_sap_coverage_result TYPE cvt_propro_tkey.


    " retrieval via testkey - this is different to other access strategy
    " as not touched programs will not be included into the result set
    cl_coverage_access=>get_prog_display_by_tky(
      EXPORTING
        im_tkey =       i_coverage_key
        im_prog_select = lt_prog_ranges
        im_devc_select = lt_devc_ranges
        im_red_yel =     '25'
        im_yel_gre =     '75'
      IMPORTING
        ex_prog_res =    rt_sap_coverage_result ).

  ENDMETHOD.                    "find_sap_coverage_results


METHOD get_instance.

    IF ( mo_instance IS INITIAL ).
      CREATE OBJECT mo_instance.
    ENDIF.
    ro_instance = mo_instance.

  ENDMETHOD.                    "get_instance


METHOD set_instance.

    mo_instance = io_dao_instance.

  ENDMETHOD.                    "set_instance
ENDCLASS.
