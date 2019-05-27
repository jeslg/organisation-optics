# organisation-optics

File contents:
* [optic](optic.scala) definitions of concrete read-only optics
* [TLinq](TLinq.scala) TLinq primitives, adapted from [Suzuki *et
  al*](http://delivery.acm.org/10.1145/2850000/2847542/p37-suzuki.pdf?ip=163.117.202.191&id=2847542&acc=ACTIVE%20SERVICE&key=DD1EC5BCF38B3699%2EAFCE2F3122C4D47C%2E4D4702B0C3E38B35%2E4D4702B0C3E38B35&__acm__=1558963736_003ff4ea03d9b9a4676fc12d88bf8d0d)
  into Scala. Includes standard semantics.
* [Optica](Optica.scala) ~~Stateless~~ Optica primitives (Getters, Affines and
  Folds). Includes interpretation in terms of `TLinq`.
* [Model](Model.scala) organisation model in terms of optics
* [Logic](Logic.scala) generic queries (`expertise`, `experts` and `experts2`)
* [nested](nested.scala) nested model, along with projection and construction
  utilities
* [schema](schema.scala) relational model, along with projection and table
  utilities
* [Main](Main.scala) exercising queries with TLinq standard semantics,
  definition of `run` and `expertsDepts`

