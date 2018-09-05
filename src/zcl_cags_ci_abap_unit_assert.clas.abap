*----------------------------------------------------------------------*
*       CLASS zcl_cags_ci_abap_unit_assert DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS zcl_cags_ci_abap_unit_assert DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS set_assert_table_result
        IMPORTING
          it_result TYPE tchar255.

    METHODS assert_equal_line
      IMPORTING
        i_expected_line TYPE string
        i_prefix_chars_to_compare TYPE i OPTIONAL.
PROTECTED SECTION.
PRIVATE SECTION.

    DATA:
        mt_assert_table_result TYPE tchar255,
        m_assert_table_counter TYPE i.
ENDCLASS.



CLASS ZCL_CAGS_CI_ABAP_UNIT_ASSERT IMPLEMENTATION.


METHOD assert_equal_line.

    DATA l_current_line TYPE string.
    DATA l_expected_line TYPE string.
    READ TABLE me->mt_assert_table_result INDEX me->m_assert_table_counter INTO l_current_line.
    ADD 1 TO  me->m_assert_table_counter.

    IF ( i_prefix_chars_to_compare IS SUPPLIED ).
      l_current_line = i_expected_line(i_prefix_chars_to_compare).
    ENDIF.

    cl_abap_unit_assert=>assert_equals(
      act = l_current_line
      exp = i_expected_line
      msg = 'Line not as expected'
    ).

  ENDMETHOD.                    "assert_equal_line


METHOD set_assert_table_result.

    me->mt_assert_table_result = it_result.
    me->m_assert_table_counter = 1.

  ENDMETHOD.                    "set_assert_result
ENDCLASS.
