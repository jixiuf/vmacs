# Copyright 2020-2021 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2

EAPI="8"

inherit cmake

if [[ "${PV}" == "99999999999999" ]]; then
	inherit git-r3

	EGIT_REPO_URI="https://github.com/lotem/librime-octagram"
else
	LIBRIME_OCTAGRAM_GIT_REVISION="7302048f12c065e0971aed3b0af46a10ec3c4cf2"
fi

DESCRIPTION="Octagram plugin for RIME"
HOMEPAGE="https://github.com/lotem/librime-octagram"
if [[ "${PV}" == "99999999999999" ]]; then
	SRC_URI=""
else
	SRC_URI="https://github.com/lotem/${PN}/archive/${LIBRIME_OCTAGRAM_GIT_REVISION}.tar.gz -> ${P}.tar.gz"
fi

LICENSE="GPL-3"
SLOT="0"
KEYWORDS=""
IUSE=""

BDEPEND=""
RDEPEND=">=app-i18n/librime-1.6:0="
DEPEND="${RDEPEND}
    dev-libs/utfcpp:0
	dev-libs/boost:0"
    

if [[ "${PV}" != "99999999999999" ]]; then
	S="${WORKDIR}/${PN}-${LIBRIME_OCTAGRAM_GIT_REVISION}"
fi

src_prepare() {
	sed \
		-e "1icmake_minimum_required(VERSION 3.0)\nproject(${PN})\n" \
		-e "s/ PARENT_SCOPE//" \
		-e "\$a\\\n" \
		-e "\$aadd_library(\${plugin_modules} MODULE \${plugin_objs})" \
		-e "\$aset_target_properties(\${plugin_modules} PROPERTIES PREFIX \"\")" \
		-e "\$atarget_link_libraries(\${plugin_modules} rime \${plugin_deps})" \
		-e "\$ainstall(TARGETS \${plugin_modules} DESTINATION $(get_libdir)/rime-plugins)" \
		-i CMakeLists.txt || die
	sed \
		-e "\$atarget_link_libraries(build_grammar glog rime \${rime_library} \${rime_dict_library})" \
		-e "\$ainstall(TARGETS build_grammar DESTINATION bin)" \
		-i tools/CMakeLists.txt || die

	cmake_src_prepare
}

src_configure() {
	local -x CXXFLAGS="${CXXFLAGS} -I${ESYSROOT}/usr/include/utf8cpp"

	cmake_src_configure
}
