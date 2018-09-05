class ZCL_CAGS_SEND_MAIL definition
  public
  final
  create public .

public section.

  methods SEND_EMAIL
    importing
      !P_SUBJECT type CSEQUENCE
      !P_TYPE type CSEQUENCE                  " Code for document class
      !P_ADDRESSES type BCSY_SMTPA
      !P_BODY_TAB type SOLI_TAB
      !P_PRIORITY type C              " Document priority (1 High - 9 Low)
      !P_PDF_LINSZ type I
      !P_SEND_IMMEDIATELY type ABAP_BOOL .
protected section.
private section.
ENDCLASS.



CLASS ZCL_CAGS_SEND_MAIL IMPLEMENTATION.


  METHOD send_email.
*  send_email(
*          EXPORTING
*            p_subject          = p_subject
*            p_type             = p_type
*            p_addresses        = p_addresses
*            p_body_tab         = p_body_tab
*            p_priority         = p_priority
*            p_pdf_linsz        = p_pdf_linsz
*            p_send_immediately = p_send_immediately ).
  ENDMETHOD.
ENDCLASS.
