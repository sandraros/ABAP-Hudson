class ZCL_USER_DAO definition
  public
  final
  create public .

public section.
class-methods get_instance RETURNING VALUE(singleton) type ref to zcl_user_dao.
methods find_user_email importing user type syuname RETURNING VALUE(email) type ad_smtpadr.

protected section.
private section.
ENDCLASS.



CLASS ZCL_USER_DAO IMPLEMENTATION.


method find_user_email.
endmethod.


  method get_instance.
    endmethod.
ENDCLASS.
