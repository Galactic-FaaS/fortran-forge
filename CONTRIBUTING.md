# Contributing to ForGE

Thank you for your interest in contributing to ForGE! We welcome contributions from the community.

## How to Contribute

### Reporting Bugs

1. Check if the bug has already been reported in [Issues](https://github.com/your-org/fortran-forge/issues)
2. If not, create a new issue with:
   - Clear title and description
   - Steps to reproduce
   - Expected vs actual behavior
   - Environment details (OS, compiler, version)
   - Code samples if applicable

### Suggesting Features

1. Check existing issues and discussions
2. Create a new issue with the `enhancement` label
3. Describe the feature and its use case
4. Discuss design and implementation approaches

### Submitting Code

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/amazing-feature`)
3. Make your changes following our coding standards
4. Add tests for new functionality
5. Update documentation as needed
6. Commit your changes (`git commit -m 'Add amazing feature'`)
7. Push to your fork (`git push origin feature/amazing-feature`)
8. Open a Pull Request

## Coding Standards

### Fortran Style

- Use Fortran 2008/2018 features
- Follow modern Fortran best practices
- Use meaningful variable and subroutine names
- Add inline documentation comments
- Format code consistently (consider using fprettify)

```fortran
!> @brief Brief description
!> @details Detailed description
!> @param arg Description of argument
!> @return Description of return value
function example_function(arg) result(res)
    integer, intent(in) :: arg
    integer :: res
    
    res = arg * 2
end function example_function
```

### Module Organization

- One module per file
- Private by default, explicitly export public symbols
- Group related procedures
- Use meaningful module names

### Error Handling

- Use `forge_status` for error reporting
- Check and handle errors appropriately
- Provide meaningful error messages

## Development Workflow

### Setting Up Development Environment

```bash
# Clone your fork
git clone https://github.com/YOUR_USERNAME/fortran-forge.git
cd fortran-forge

# Add upstream remote
git remote add upstream https://github.com/your-org/fortran-forge.git

# Install dependencies (example for Ubuntu)
sudo apt install gfortran cmake

# Install fpm
# See: https://fpm.fortran-lang.org/install/

# Build
fpm build
```

### Building and Testing

```bash
# Build with fpm
fpm build --profile dev

# Run tests (when implemented)
fpm test

# Build with CMake
mkdir build && cd build
cmake .. -DFORGE_BUILD_TESTS=ON
cmake --build .
ctest
```

### Documentation

- Update relevant documentation files
- Add/update inline API documentation
- Update CHANGELOG.md for user-facing changes
- Include code examples for new features

## Priority Areas

We're especially interested in contributions in these areas:

1. **Tcl/Tk Backend Implementation** - Primary backend development
2. **Widget Implementation** - Additional widget types
3. **Layout Managers** - Implementing layout functionality
4. **Testing** - Unit and integration tests
5. **Documentation** - Tutorials, examples, API docs
6. **CI/CD** - Improving build and test automation
7. **Examples** - More demo applications
8. **GTK4/Qt Backends** - Alternative backend implementations

## Questions?

- Open a [Discussion](https://github.com/your-org/fortran-forge/discussions)
- Check existing documentation in `docs/`
- Contact maintainers via email

## License

By contributing to ForGE, you agree that your contributions will be licensed under the GNU General Public License v3.0 or later.

Thank you for contributing to ForGE!

