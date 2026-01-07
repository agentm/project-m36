# Role-Based Access Control

Project:M36 supports fine-grained access control which is slightly different from DBMSes like PostgreSQL or Oracle. Fundamentally, Project:M36 enforces two types of DBMS users: those who can view and manipulate relation variables (similar to tables in SQL) and those who manipulate the database through functions, effectively creating an application API layer within the database. By default, new roles are not granted *any* permissions.

## Login Roles

Login roles control which roles can connect to the database. To map connections to roles, Project:M36 uses mutual TLS certificate authentication.

A login role is composed of a name and a flag indicating whether the user can login. Roles which cannot login can be enabled later.

```
TutorialD (master/main): :addloginrole steve maynotlogin
TutorialD (master/main): :alterrolemaylogin steve maylogin
```

An administrator can view all login roles.

```
TutorialD (master/main): :showloginroles
00000000-0000-0000-0000-000000000000:admin:maylogin
48b179af-b697-4907-8f8d-e0cada746f48:steve:maylogin
```

Login roles can have any number of permissions. There are currently two supported permissions: `view_login_roles` and `alter_login_roles`.

```
TutorialD (master/main): :addpermissiontologinrole steve view_login_roles nogrant
```

The scope of login role permissions is immediate and outside of database transactions. This allows a database administrator, for example, to revoke access to the entire database without consideration for which transactions the role may have historically had access.

## Relation Variable Access

Unlike SQL databases, roles have all or nothing access to the underlying relation variables. Ideally, database developers develop the relation variables and accompanying database schema while database users manipulate data using the database functions (see below).

```
TutorialD (master/main): grant steve accessrelvars maygrant
```
or

```
TutorialD (master/main): revoke steve accessrelvars
```

The scope of this permission is transactional. Therefore, a role which has had this permission and then had it revoked can still manipulate the relation variables of a past transaction where the role had the permission. This permission is part of the transaction state and cannot be revoked from past transactions.

## Function Access

The function permissions allow roles to access, view, execute, or load functions in the database. The grant boolean flag indicates whether or not the role is allowed to grant the permission to other roles.

```
TutorialD (master/main): grant steve executefunctions nogrant
TutorialD (master/main): grant steve viewfunctions nogrant
TutorialD (master/main): grant steve loadfunctions nogrant
```

```
TutorialD (master/main): revoke steve executefunctions
TutorialD (master/main): revoke steve viewfunctions
TutorialD (master/main): revoke steve loadfunctions
```

The scope of this permission is transactional. Therefore, a role which has had this permission and then had it revoked can still manipulate the functions of a past transaction where the role had the permission and branch the database state if the role has the commit permission. This permission is part of the transaction state and cannot be revoked from past transactions.

## Commit Access

Controlling the commit permission allows one to create read-only roles. Without this permission, the role cannot alter the database.


```
TutorialD (master/main): grant steve committransaction nogrant
TutorialD (master/main): revoke steve committransaction
```

The scope of this permission is transactional. Therefore, a role which has had this permission and then had it revoked can still branch the database from a past transaction where the role had the permission if the role has the commit permission. This permission is part of the transaction state and cannot be revoked from past transactions.

## Alter Schema Access

A role may be granted permission to read the underlying relation variables without being able to alter the database schema using the `alterschema` permission.

```
TutorialD (master/main): grant steve alterschema nogrant
```

```
TutorialD (master/main): revoke steve alterschema
```

The scope of this permission is transactional. Therefore, a role which has had this permission and then had it revoked can still branch the database from a past transaction where the role had the permission if the role has the commit permission. This permission is part of the transaction state and cannot be revoked from past transactions.

## Access Control Access

By default, the `admin` role is responsible for manipulating database permissions. Use the ACL permissions to delegate this privilege.

```
TutorialD (master/main): grant steve alteracl nogrant
TutorialD (master/main): grant steve viewacl nogrant
```

```
TutorialD (master/main): revoke steve alteracl 
TutorialD (master/main): revoke steve viewacl
```

The scope of this permission is transactional. Therefore, a role which has had this permission and then had it revoked can still branch the database from a past transaction where the role had the permission if the role has the commit permission. This permission is part of the transaction state and cannot be revoked from past transactions.

## Database Context Function Access

Individual database context function access are controlled by three permissions: `viewfunction`, `executefunction`, and `alterfunction`.

```
TutorialD (master/main): grant dbcfunction steve deleteAll viewfunction nogrant
TutorialD (master/main): grant dbcfunction steve deleteAll executefunction nogrant
TutorialD (master/main): grant dbcfunction steve deleteAll alterfunction nogrant
```

```
TutorialD (master/main): revoke dbcfunction steve deleteAll viewfunction
TutorialD (master/main): revoke dbcfunction steve deleteAll executefunction
TutorialD (master/main): revoke dbcfunction steve deleteAll alterfunction
```

The scope of this permission is transactional. Therefore, a role which has had this permission and then had it revoked can still access a function from that past transaction. This permission is part of the transaction state and cannot be revoked from past transactions.

# Conclusion

Project:M36 supports fine-grained access control but it is different from how typical SQL DBMSes implement security. The difference in access control is based on eliminating information leaks and data anomalies when applying row-based access control (which Project:M36 explicitly does *not* support). For more information, read about the impetus behind the [security design]().

Note that many of the permissions are frozen into transactional state. Since Project:M36 support time-travel-like features, the state of past transactions cannot be altered. To permanently revoke all permissions from a user, the admin can prevent the original rolename from logging into the database and replace it with a new role. The role can even have the same original role name.