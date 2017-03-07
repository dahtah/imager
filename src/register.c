#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP imager_autocrop_(SEXP, SEXP, SEXP);
extern SEXP imager_bdilate(SEXP, SEXP, SEXP);
extern SEXP imager_bdilate_rect(SEXP, SEXP, SEXP, SEXP);
extern SEXP imager_bdilate_square(SEXP, SEXP);
extern SEXP imager_bdistance_transform(SEXP, SEXP, SEXP);
extern SEXP imager_berode(SEXP, SEXP, SEXP);
extern SEXP imager_berode_rect(SEXP, SEXP, SEXP, SEXP);
extern SEXP imager_berode_square(SEXP, SEXP);
extern SEXP imager_blabel(SEXP, SEXP);
extern SEXP imager_blur_anisotropic(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP imager_boxblur(SEXP, SEXP, SEXP);
extern SEXP imager_boxblur_xy(SEXP, SEXP, SEXP, SEXP);
extern SEXP imager_bucket_fill(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP imager_bucket_select(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP imager_checkcoords(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP imager_cimg_omp();
extern SEXP imager_convolve(SEXP, SEXP, SEXP, SEXP);
extern SEXP imager_correlate(SEXP, SEXP, SEXP, SEXP);
extern SEXP imager_deriche(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP imager_diffusion_tensors(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP imager_dilate(SEXP, SEXP, SEXP, SEXP);
extern SEXP imager_dilate_rect(SEXP, SEXP, SEXP, SEXP);
extern SEXP imager_dilate_square(SEXP, SEXP);
extern SEXP imager_displacement(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP imager_display_(SEXP, SEXP);
extern SEXP imager_display_list(SEXP);
extern SEXP imager_distance_transform(SEXP, SEXP, SEXP);
extern SEXP imager_do_patchmatch(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP imager_draw_image(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP imager_erode(SEXP, SEXP, SEXP, SEXP);
extern SEXP imager_erode_rect(SEXP, SEXP, SEXP, SEXP);
extern SEXP imager_erode_square(SEXP, SEXP);
extern SEXP imager_extract_fast(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP imager_extract_patches(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP imager_extract_patches3D(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP imager_FFT_complex(SEXP, SEXP, SEXP, SEXP);
extern SEXP imager_FFT_realim(SEXP, SEXP, SEXP);
extern SEXP imager_FFT_realout(SEXP, SEXP, SEXP, SEXP);
extern SEXP imager_get_gradient(SEXP, SEXP, SEXP);
extern SEXP imager_get_hessian(SEXP, SEXP);
extern SEXP imager_getCc(SEXP, SEXP, SEXP, SEXP);
extern SEXP imager_getXc(SEXP, SEXP, SEXP, SEXP);
extern SEXP imager_getYc(SEXP, SEXP, SEXP, SEXP);
extern SEXP imager_getZc(SEXP, SEXP, SEXP, SEXP);
extern SEXP imager_haar(SEXP, SEXP, SEXP);
extern SEXP imager_has_omp();
extern SEXP imager_HSItoRGB(SEXP);
extern SEXP imager_HSLtoRGB(SEXP);
extern SEXP imager_HSVtoRGB(SEXP);
extern SEXP imager_im_append(SEXP, SEXP);
extern SEXP imager_im_split(SEXP, SEXP, SEXP);
extern SEXP imager_imeval(SEXP, SEXP);
extern SEXP imager_imlap(SEXP);
extern SEXP imager_imshift(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP imager_interp_xy(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP imager_interp_xyc(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP imager_interp_xyz(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP imager_interp_xyzc(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP imager_isoblur(SEXP, SEXP, SEXP, SEXP);
extern SEXP imager_label(SEXP, SEXP, SEXP);
extern SEXP imager_LabtoRGB(SEXP);
extern SEXP imager_LabtosRGB(SEXP);
extern SEXP imager_LabtoXYZ(SEXP);
extern SEXP imager_load_image(SEXP);
extern SEXP imager_mclosing(SEXP, SEXP, SEXP, SEXP);
extern SEXP imager_mclosing_square(SEXP, SEXP);
extern SEXP imager_medianblur(SEXP, SEXP, SEXP);
extern SEXP imager_mirror(SEXP, SEXP);
extern SEXP imager_mopening(SEXP, SEXP, SEXP, SEXP);
extern SEXP imager_mopening_square(SEXP, SEXP);
extern SEXP imager_patch_summary_cimg(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP imager_periodic_part(SEXP);
extern SEXP imager_permute_axes(SEXP, SEXP);
extern SEXP imager_play(SEXP, SEXP, SEXP, SEXP);
extern SEXP imager_porder(SEXP, SEXP);
extern SEXP imager_prank(SEXP, SEXP);
extern SEXP imager_psort(SEXP, SEXP);
extern SEXP imager_px_append(SEXP, SEXP);
extern SEXP imager_px_split(SEXP, SEXP, SEXP);
extern SEXP imager_reduce_average(SEXP, SEXP);
extern SEXP imager_reduce_list(SEXP, SEXP);
extern SEXP imager_reduce_list2(SEXP, SEXP);
extern SEXP imager_reduce_med(SEXP, SEXP);
extern SEXP imager_reduce_minmax(SEXP, SEXP, SEXP);
extern SEXP imager_reduce_prod(SEXP, SEXP);
extern SEXP imager_reduce_wsum(SEXP, SEXP, SEXP);
extern SEXP imager_resize(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP imager_resize_doubleXY(SEXP);
extern SEXP imager_resize_halfXY(SEXP);
extern SEXP imager_resize_tripleXY(SEXP);
extern SEXP imager_RGBtoHSI(SEXP);
extern SEXP imager_RGBtoHSL(SEXP);
extern SEXP imager_RGBtoHSV(SEXP);
extern SEXP imager_RGBtoLab(SEXP);
extern SEXP imager_RGBtosRGB(SEXP);
extern SEXP imager_RGBtoXYZ(SEXP);
extern SEXP imager_RGBtoYCbCr(SEXP);
extern SEXP imager_RGBtoYUV(SEXP);
extern SEXP imager_rotate(SEXP, SEXP, SEXP, SEXP);
extern SEXP imager_rotate_xy(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP imager_save_image(SEXP, SEXP);
extern SEXP imager_select(SEXP, SEXP);
extern SEXP imager_set_cimg_omp(SEXP);
extern SEXP imager_sharpen(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP imager_sRGBtoLab(SEXP);
extern SEXP imager_sRGBtoRGB(SEXP);
extern SEXP imager_vanvliet(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP imager_warp(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP imager_watershed(SEXP, SEXP, SEXP);
extern SEXP imager_XYZtoLab(SEXP);
extern SEXP imager_XYZtoRGB(SEXP);
extern SEXP imager_YCbCrtoRGB(SEXP);
extern SEXP imager_YUVtoRGB(SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"imager_autocrop_",           (DL_FUNC) &imager_autocrop_,            3},
    {"imager_bdilate",             (DL_FUNC) &imager_bdilate,              3},
    {"imager_bdilate_rect",        (DL_FUNC) &imager_bdilate_rect,         4},
    {"imager_bdilate_square",      (DL_FUNC) &imager_bdilate_square,       2},
    {"imager_bdistance_transform", (DL_FUNC) &imager_bdistance_transform,  3},
    {"imager_berode",              (DL_FUNC) &imager_berode,               3},
    {"imager_berode_rect",         (DL_FUNC) &imager_berode_rect,          4},
    {"imager_berode_square",       (DL_FUNC) &imager_berode_square,        2},
    {"imager_blabel",              (DL_FUNC) &imager_blabel,               2},
    {"imager_blur_anisotropic",    (DL_FUNC) &imager_blur_anisotropic,    11},
    {"imager_boxblur",             (DL_FUNC) &imager_boxblur,              3},
    {"imager_boxblur_xy",          (DL_FUNC) &imager_boxblur_xy,           4},
    {"imager_bucket_fill",         (DL_FUNC) &imager_bucket_fill,          8},
    {"imager_bucket_select",       (DL_FUNC) &imager_bucket_select,        6},
    {"imager_checkcoords",         (DL_FUNC) &imager_checkcoords,          5},
    {"imager_cimg_omp",            (DL_FUNC) &imager_cimg_omp,             0},
    {"imager_convolve",            (DL_FUNC) &imager_convolve,             4},
    {"imager_correlate",           (DL_FUNC) &imager_correlate,            4},
    {"imager_deriche",             (DL_FUNC) &imager_deriche,              5},
    {"imager_diffusion_tensors",   (DL_FUNC) &imager_diffusion_tensors,    6},
    {"imager_dilate",              (DL_FUNC) &imager_dilate,               4},
    {"imager_dilate_rect",         (DL_FUNC) &imager_dilate_rect,          4},
    {"imager_dilate_square",       (DL_FUNC) &imager_dilate_square,        2},
    {"imager_displacement",        (DL_FUNC) &imager_displacement,         7},
    {"imager_display_",            (DL_FUNC) &imager_display_,             2},
    {"imager_display_list",        (DL_FUNC) &imager_display_list,         1},
    {"imager_distance_transform",  (DL_FUNC) &imager_distance_transform,   3},
    {"imager_do_patchmatch",       (DL_FUNC) &imager_do_patchmatch,        8},
    {"imager_draw_image",          (DL_FUNC) &imager_draw_image,           6},
    {"imager_erode",               (DL_FUNC) &imager_erode,                4},
    {"imager_erode_rect",          (DL_FUNC) &imager_erode_rect,           4},
    {"imager_erode_square",        (DL_FUNC) &imager_erode_square,         2},
    {"imager_extract_fast",        (DL_FUNC) &imager_extract_fast,         6},
    {"imager_extract_patches",     (DL_FUNC) &imager_extract_patches,      5},
    {"imager_extract_patches3D",   (DL_FUNC) &imager_extract_patches3D,    7},
    {"imager_FFT_complex",         (DL_FUNC) &imager_FFT_complex,          4},
    {"imager_FFT_realim",          (DL_FUNC) &imager_FFT_realim,           3},
    {"imager_FFT_realout",         (DL_FUNC) &imager_FFT_realout,          4},
    {"imager_get_gradient",        (DL_FUNC) &imager_get_gradient,         3},
    {"imager_get_hessian",         (DL_FUNC) &imager_get_hessian,          2},
    {"imager_getCc",               (DL_FUNC) &imager_getCc,                4},
    {"imager_getXc",               (DL_FUNC) &imager_getXc,                4},
    {"imager_getYc",               (DL_FUNC) &imager_getYc,                4},
    {"imager_getZc",               (DL_FUNC) &imager_getZc,                4},
    {"imager_haar",                (DL_FUNC) &imager_haar,                 3},
    {"imager_has_omp",             (DL_FUNC) &imager_has_omp,              0},
    {"imager_HSItoRGB",            (DL_FUNC) &imager_HSItoRGB,             1},
    {"imager_HSLtoRGB",            (DL_FUNC) &imager_HSLtoRGB,             1},
    {"imager_HSVtoRGB",            (DL_FUNC) &imager_HSVtoRGB,             1},
    {"imager_im_append",           (DL_FUNC) &imager_im_append,            2},
    {"imager_im_split",            (DL_FUNC) &imager_im_split,             3},
    {"imager_imeval",              (DL_FUNC) &imager_imeval,               2},
    {"imager_imlap",               (DL_FUNC) &imager_imlap,                1},
    {"imager_imshift",             (DL_FUNC) &imager_imshift,              6},
    {"imager_interp_xy",           (DL_FUNC) &imager_interp_xy,            6},
    {"imager_interp_xyc",          (DL_FUNC) &imager_interp_xyc,           6},
    {"imager_interp_xyz",          (DL_FUNC) &imager_interp_xyz,           6},
    {"imager_interp_xyzc",         (DL_FUNC) &imager_interp_xyzc,          6},
    {"imager_isoblur",             (DL_FUNC) &imager_isoblur,              4},
    {"imager_label",               (DL_FUNC) &imager_label,                3},
    {"imager_LabtoRGB",            (DL_FUNC) &imager_LabtoRGB,             1},
    {"imager_LabtosRGB",           (DL_FUNC) &imager_LabtosRGB,            1},
    {"imager_LabtoXYZ",            (DL_FUNC) &imager_LabtoXYZ,             1},
    {"imager_load_image",          (DL_FUNC) &imager_load_image,           1},
    {"imager_mclosing",            (DL_FUNC) &imager_mclosing,             4},
    {"imager_mclosing_square",     (DL_FUNC) &imager_mclosing_square,      2},
    {"imager_medianblur",          (DL_FUNC) &imager_medianblur,           3},
    {"imager_mirror",              (DL_FUNC) &imager_mirror,               2},
    {"imager_mopening",            (DL_FUNC) &imager_mopening,             4},
    {"imager_mopening_square",     (DL_FUNC) &imager_mopening_square,      2},
    {"imager_patch_summary_cimg",  (DL_FUNC) &imager_patch_summary_cimg,   6},
    {"imager_periodic_part",       (DL_FUNC) &imager_periodic_part,        1},
    {"imager_permute_axes",        (DL_FUNC) &imager_permute_axes,         2},
    {"imager_play",                (DL_FUNC) &imager_play,                 4},
    {"imager_porder",              (DL_FUNC) &imager_porder,               2},
    {"imager_prank",               (DL_FUNC) &imager_prank,                2},
    {"imager_psort",               (DL_FUNC) &imager_psort,                2},
    {"imager_px_append",           (DL_FUNC) &imager_px_append,            2},
    {"imager_px_split",            (DL_FUNC) &imager_px_split,             3},
    {"imager_reduce_average",      (DL_FUNC) &imager_reduce_average,       2},
    {"imager_reduce_list",         (DL_FUNC) &imager_reduce_list,          2},
    {"imager_reduce_list2",        (DL_FUNC) &imager_reduce_list2,         2},
    {"imager_reduce_med",          (DL_FUNC) &imager_reduce_med,           2},
    {"imager_reduce_minmax",       (DL_FUNC) &imager_reduce_minmax,        3},
    {"imager_reduce_prod",         (DL_FUNC) &imager_reduce_prod,          2},
    {"imager_reduce_wsum",         (DL_FUNC) &imager_reduce_wsum,          3},
    {"imager_resize",              (DL_FUNC) &imager_resize,              11},
    {"imager_resize_doubleXY",     (DL_FUNC) &imager_resize_doubleXY,      1},
    {"imager_resize_halfXY",       (DL_FUNC) &imager_resize_halfXY,        1},
    {"imager_resize_tripleXY",     (DL_FUNC) &imager_resize_tripleXY,      1},
    {"imager_RGBtoHSI",            (DL_FUNC) &imager_RGBtoHSI,             1},
    {"imager_RGBtoHSL",            (DL_FUNC) &imager_RGBtoHSL,             1},
    {"imager_RGBtoHSV",            (DL_FUNC) &imager_RGBtoHSV,             1},
    {"imager_RGBtoLab",            (DL_FUNC) &imager_RGBtoLab,             1},
    {"imager_RGBtosRGB",           (DL_FUNC) &imager_RGBtosRGB,            1},
    {"imager_RGBtoXYZ",            (DL_FUNC) &imager_RGBtoXYZ,             1},
    {"imager_RGBtoYCbCr",          (DL_FUNC) &imager_RGBtoYCbCr,           1},
    {"imager_RGBtoYUV",            (DL_FUNC) &imager_RGBtoYUV,             1},
    {"imager_rotate",              (DL_FUNC) &imager_rotate,               4},
    {"imager_rotate_xy",           (DL_FUNC) &imager_rotate_xy,            6},
    {"imager_save_image",          (DL_FUNC) &imager_save_image,           2},
    {"imager_select",              (DL_FUNC) &imager_select,               2},
    {"imager_set_cimg_omp",        (DL_FUNC) &imager_set_cimg_omp,         1},
    {"imager_sharpen",             (DL_FUNC) &imager_sharpen,              6},
    {"imager_sRGBtoLab",           (DL_FUNC) &imager_sRGBtoLab,            1},
    {"imager_sRGBtoRGB",           (DL_FUNC) &imager_sRGBtoRGB,            1},
    {"imager_vanvliet",            (DL_FUNC) &imager_vanvliet,             5},
    {"imager_warp",                (DL_FUNC) &imager_warp,                 5},
    {"imager_watershed",           (DL_FUNC) &imager_watershed,            3},
    {"imager_XYZtoLab",            (DL_FUNC) &imager_XYZtoLab,             1},
    {"imager_XYZtoRGB",            (DL_FUNC) &imager_XYZtoRGB,             1},
    {"imager_YCbCrtoRGB",          (DL_FUNC) &imager_YCbCrtoRGB,           1},
    {"imager_YUVtoRGB",            (DL_FUNC) &imager_YUVtoRGB,             1},
    {NULL, NULL, 0}
};

void R_init_imager(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
