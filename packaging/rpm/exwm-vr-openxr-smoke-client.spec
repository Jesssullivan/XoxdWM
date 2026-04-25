# RPM spec for the EXWM-VR OpenXR smoke client
# Target: Rocky Linux 10

%global sdk_commit b8e48244207d9a623649e4de8cdbe288997194c1
%global sdk_date   20260425
%global project_name exwm-vr

Name:           exwm-vr-openxr-smoke-client
Version:        0.0.1
Release:        1.%{sdk_date}git%{?dist}
Summary:        OpenXR smoke client for EXWM-VR host repeatability checks
License:        Apache-2.0
URL:            https://github.com/KhronosGroup/OpenXR-SDK-Source
Source0:        https://github.com/KhronosGroup/OpenXR-SDK-Source/archive/%{sdk_commit}/OpenXR-SDK-Source-%{sdk_commit}.tar.gz

BuildRequires:  cmake >= 3.16
BuildRequires:  ninja-build
BuildRequires:  gcc-c++
BuildRequires:  python3
BuildRequires:  pkgconfig
BuildRequires:  openxr-devel
BuildRequires:  vulkan-headers
BuildRequires:  vulkan-loader-devel
BuildRequires:  mesa-libGL-devel
BuildRequires:  libX11-devel
BuildRequires:  libXrandr-devel
BuildRequires:  libXxf86vm-devel

Requires:       openxr-libs
Requires:       vulkan-loader
Requires:       libX11
Requires:       libXrandr
Requires:       libXxf86vm

%description
This package builds the Khronos OpenXR SDK hello_xr sample as a bounded smoke
client for EXWM-VR host repeatability checks. It installs the binary under the
EXWM-VR libexec tree and exposes an exwm-vr-hello-xr command name so the
operator wrapper can prefer a repo-managed client path over host-local
/usr/local binaries.

%prep
%autosetup -n OpenXR-SDK-Source-%{sdk_commit}

%build
%cmake -G Ninja \
    -DCMAKE_BUILD_TYPE=Release \
    -DCMAKE_SKIP_RPATH=ON \
    -DCMAKE_SKIP_BUILD_RPATH=ON \
    -DCMAKE_BUILD_WITH_INSTALL_RPATH=OFF \
    -DCMAKE_INSTALL_RPATH_USE_LINK_PATH=OFF \
    -DBUILD_API_LAYERS=OFF \
    -DBUILD_CONFORMANCE_TESTS=OFF \
    -DBUILD_TESTS=ON \
    -DBUILD_SDK_TESTS=ON \
    -DPRESENTATION_BACKEND=xlib
%cmake_build --target hello_xr

%install
install -Dpm 0755 %{__cmake_builddir}/src/tests/hello_xr/hello_xr \
    %{buildroot}%{_libexecdir}/%{project_name}/hello_xr
install -d %{buildroot}%{_bindir}
ln -s ../libexec/%{project_name}/hello_xr \
    %{buildroot}%{_bindir}/exwm-vr-hello-xr

%files
%license LICENSE
%doc README.md
%{_bindir}/exwm-vr-hello-xr
%{_libexecdir}/%{project_name}/hello_xr

%changelog
* Sat Apr 25 2026 EXWM-VR <noreply@example.com> - 0.0.1-1
- Initial package: Khronos hello_xr smoke client for honey repeatability.
