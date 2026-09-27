---
NOTE:
  This is a managed document.
  The content should be precise and succinct, in RFC style.
  No verbose essays.
  This document is intended to help both human readers and guide AI agents.
  An AI agent MUST NOT automatically edit this file.
---

# Best practices

General:
- Use UPPERCASE for PostgreSQL keywords, including type names.

Data modeling:
- Prefer column constraints instead of table constraints.
- **NEVER** use `TIMESTAMP` without a timezone.
- Prefer `UUID` (UUID v7) primary keys over numeric primary keys.
- Use singular table names instead of plural table names.
  e.g. `user` instead of `users`.

References:

- [PostgreSQL wiki: Don't do this](https://wiki.postgresql.org/wiki/Don%27t_Do_This)
- [Bytebase: PostgreSQL SQL Review and Style Guide](https://www.bytebase.com/blog/postgres-sql-review-guide/#design-rules)
- [Tiger Data: Guide to PostgreSQL database design](https://www.tigerdata.com/learn/guide-to-postgresql-database-design)
- [Future Architect: PostgreSQL設計ガイドライン](https://future-architect.github.io/arch-guidelines/documents/forDB/postgresql_guidelines.html)
- [Life Altering PostgreSQL patterns](https://mccue.dev/pages/3-11-25-life-altering-postgresql-patterns)
