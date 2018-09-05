*----------------------------------------------------------------------*
*       CLASS zcx_cags_ci_report_exception DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS zcx_cags_ci_report_exception DEFINITION
  PUBLIC
  INHERITING FROM cx_dynamic_check
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.


    CLASS-METHODS create
      IMPORTING i_code  TYPE sy-subrc  OPTIONAL
                i_msg   TYPE string OPTIONAL
      RETURNING
        value(ro_exception) TYPE REF TO zcx_cags_ci_report_exception.

    CLASS-METHODS raise_exception
      IMPORTING
        i_code TYPE sy-subrc DEFAULT '0'
        i_msg  TYPE string
      raising zcx_cags_ci_report_exception
        .

    METHODS get_msg_with_code
          RETURNING value(r_msg) TYPE string.
PRIVATE SECTION.
    DATA m_error_code TYPE sy-subrc.
    DATA m_msg TYPE string.
ENDCLASS.



CLASS ZCX_CAGS_CI_REPORT_EXCEPTION IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
TEXTID = TEXTID
PREVIOUS = PREVIOUS
.
  endmethod.


METHOD create.

    CREATE OBJECT ro_exception.
    ro_exception->m_error_code = i_code.
    ro_exception->m_msg = i_msg.

  ENDMETHOD.                    "constructor


METHOD get_msg_with_code.
    r_msg = m_msg && '. Error code=' && m_error_code.
  ENDMETHOD.                    "get_msg_with_code


METHOD raise_exception.

    DATA lo_exc TYPE REF TO zcx_cags_ci_report_exception.
    lo_exc = create(
        i_code = i_code
        i_msg  = i_msg
    ).
    RAISE EXCEPTION lo_exc.

  ENDMETHOD.                    "raise_exception
ENDCLASS.
