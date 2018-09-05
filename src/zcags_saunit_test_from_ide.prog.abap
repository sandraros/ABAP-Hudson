*&---------------------------------------------------------------------*
*& Report  ZCAGS_SAUNIT_TEST_FROM_IDE
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZCAGS_SAUNIT_TEST_FROM_IDE.

  type-pools:
    abap.

  parameters:
    p_Keys       type sabp_T_Tadir_Keys no-display,
    p_Cvrg       type flag              no-display,
    p_Durat      type saunit_D_Allowed_Rt_Duration  no-display,
    p_Risk       type saunit_D_Allowed_Risk_Level   no-display.

  rollback work.
  call function '__SABP_AU_TEST_ITEMS_FROM_IDE'
    exporting
      tadir_Keys  =               p_Keys
      with_Coverage =             p_Cvrg
      limit_On_Duration_Category =  p_Durat
      limit_On_Risk_Level =         p_Risk.
  commit work.
