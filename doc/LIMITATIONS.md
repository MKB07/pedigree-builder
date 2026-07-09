# Limitations

## Clinical Use

The application is not validated as a clinical device. It is intended for education, research and prototyping only.

## Data Privacy

The repository must not contain identifiable patient data. Example pedigrees should remain synthetic, anonymized or public-domain teaching examples.

## External Resources

Some features query external biomedical resources. These features can fail when:

- internet access is unavailable;
- an external API changes;
- an external service rate-limits requests;
- a resource is removed or renamed.

## Computational Scope

Large pedigrees may require additional optimization for rendering, scaling and relationship calculations. Plot layout is especially sensitive to window size, label size and dense family structures.

## Validation

The current test coverage is manual. Future work should add automated checks for:

- pedigree creation and mutation helpers;
- relationship-table calculations;
- plot scaling fallbacks;
- import/export behavior;
- API failure handling.
