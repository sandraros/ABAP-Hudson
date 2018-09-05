*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
CLASS lcl_ci_test_data_mock DEFINITION inheriting from zcl_cags_ci_test_data.
public section.

 TYPES BEGIN OF lty_method_call_details.
  TYPES !class_name TYPE string.
  TYPES !local_test_class TYPE string.
  TYPES !method_name TYPE string.
  TYPES !alert_info TYPE string .
  TYPES END OF lty_method_call_details.


  methods add_failed_method FINAL redefinition.

  methods add_test_method FINAL redefinition.

  methods get_first_failed_method
    RETURNING VALUE(rs_methods) TYPE lty_method_call_details.

  methods get_first_success_method
  RETURNING VALUE(rs_methods) TYPE lty_method_call_details.

PRIVATE SECTION.



  DATA mt_failed_methods TYPE table of lty_method_call_details .
  DATA mt_success_methods TYPE table of  lty_method_call_details.
  DATA ms_method TYPE lty_method_call_details.



ENDCLASS.

CLASS lcl_ci_test_data_mock implementation.

  method add_failed_method.

    clear ms_method.
    ms_method-class_name = i_class_name.
    ms_method-local_test_class = i_local_test_class.
    ms_method-method_name = i_method_name.
    ms_method-alert_info = i_alert_msg.
    APPEND ms_method to mt_failed_methods.

  endmethod.

  METHOD add_test_method.

    CLEAR ms_method.
    ms_method-class_name = i_class_name.
    ms_method-local_test_class = i_local_test_class.
    ms_method-method_name = i_method_name.
    APPEND ms_method TO mt_success_methods.

  ENDMETHOD.


  METHOD get_first_failed_method.
  clear ms_method.
  READ TABLE mt_failed_methods INDEX 1 into ms_method.
    rs_methods = ms_method.
  ENDMETHOD.

  METHOD get_first_success_method.
    CLEAR ms_method.
    READ TABLE mt_success_methods INDEX 1 INTO ms_method.
    rs_methods = ms_method.
  ENDMETHOD.
endclass.
