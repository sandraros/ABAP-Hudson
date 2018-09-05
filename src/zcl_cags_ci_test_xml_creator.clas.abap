*----------------------------------------------------------------------*
*       CLASS ZCL_CAGS_CI_TEST_XML_CREATOR DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS zcl_cags_ci_test_xml_creator DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS open .
    METHODS add_success_method
      IMPORTING
        !i_package TYPE string
        !i_user TYPE uname
        !i_class_name TYPE string
        !i_method_name TYPE string .
    METHODS add_failure_method
      IMPORTING
        !i_package TYPE string
        !i_user TYPE uname
        !i_class_name TYPE string
        !i_method_name TYPE string
        !i_failure_msg TYPE string
        !i_failure_type TYPE string .
    METHODS close .

    METHODS get_xml_content_package
       RETURNING
         value(rt_tests_result) TYPE tchar255.

    METHODS get_xml_content_user
      RETURNING
        value(rt_tests_result) TYPE tchar255.
PROTECTED SECTION.
PRIVATE SECTION.

    DATA mt_xml_content_package_tests TYPE tchar255 .
    DATA mt_xml_content_user_tests TYPE tchar255 .
    CLASS-DATA c_local_hudson_xml_file_packag TYPE string VALUE 'C:\APPL\CI\JOBS\JUNIT_PACKAGE_RESULTS\PACKAGE_RESULTS.XML'. "#EC NOTEXT .  .  .  .  .  .  . " .
    CLASS-DATA c_local_hudson_xml_file_user TYPE string VALUE 'C:\APPL\CI\JOBS\JUNIT_USER_RESULTS\USER_RESULTS.XML'. "#EC NOTEXT .  .  .  .  .  .  . " .
    CLASS-DATA c_local_hudson_xml_file_manual TYPE string VALUE 'C:\APPL\CI\JOBS\JUNIT_RESULTS\MANUAL_RESULTS.XML'. "#EC NOTEXT .
ENDCLASS.



CLASS ZCL_CAGS_CI_TEST_XML_CREATOR IMPLEMENTATION.


METHOD add_failure_method.

    DATA l_line TYPE string.
    DATA l_unescaped_failure_msg TYPE string.
    l_line = |   <testcase classname="{ i_package }.{ i_class_name }" name="{ i_method_name }" time="0.0" >|.
    APPEND l_line TO mt_xml_content_package_tests.
    l_line = |   <testcase classname="{ i_user }.{ i_class_name }" name="{ i_method_name }" time="0.0" >|.
    APPEND l_line TO mt_xml_content_user_tests.

    l_unescaped_failure_msg = i_failure_msg.
    REPLACE ALL OCCURRENCES OF '<' IN l_unescaped_failure_msg WITH '&lt;'.
    REPLACE ALL OCCURRENCES OF '>' IN l_unescaped_failure_msg WITH '&gt;'.
    l_line = |      <failure message="{ l_unescaped_failure_msg }" type="{ i_failure_type }">Error found.</failure>|.
    APPEND l_line TO mt_xml_content_package_tests.
    APPEND l_line TO mt_xml_content_user_tests.

    l_line = |   </testcase>|.
    APPEND l_line TO mt_xml_content_package_tests.
    APPEND l_line TO mt_xml_content_user_tests.

  ENDMETHOD.                    "ADD_FAILURE_METHOD


METHOD add_success_method.

    DATA l_line TYPE string.
    l_line = |   <testcase classname="{ i_package }.{ i_class_name }" name="{ i_method_name }" time="0.0" />|.
    APPEND l_line TO mt_xml_content_package_tests.

    l_line = |   <testcase classname="{ i_user }.{ i_class_name }" name="{ i_method_name }" time="0.0" />|.
    APPEND l_line TO mt_xml_content_user_tests.

  ENDMETHOD.                    "add_success_method


METHOD close.

    DATA l_line TYPE string.

    l_line = |</testsuite>|.
    APPEND l_line TO mt_xml_content_package_tests.
    APPEND l_line TO mt_xml_content_user_tests.

  ENDMETHOD.                    "CLOSE


METHOD get_xml_content_package.

    rt_tests_result = mt_xml_content_package_tests.

  ENDMETHOD.                    "get_xml_content_package


METHOD get_xml_content_user.

    rt_tests_result = mt_xml_content_user_tests.

  ENDMETHOD.                    "get_xml_content_user


METHOD open.

    DATA l_line TYPE string.
    DATA l_datetime TYPE string.

    CLEAR mt_xml_content_package_tests.

    l_datetime = |{ sy-datum } { sy-uzeit }|.

    l_line = |<?xml version="1.0" encoding="UTF-8" ?>|.
    APPEND l_line TO mt_xml_content_package_tests.
    APPEND l_line TO mt_xml_content_user_tests.
    l_line = |<testsuite created="{ l_datetime }">|.
    APPEND l_line TO mt_xml_content_package_tests.
    APPEND l_line TO mt_xml_content_user_tests.

  ENDMETHOD.                    "OPEN
ENDCLASS.
