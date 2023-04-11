tic % initiate timer

%% set-up options and do the thing
lead path;
options = getoptslocal;
ea_run('run', options);
time = toc; % finish the timer
sec = seconds(time); % reformat the finish time to seconds
sec.Format = 'hh:mm:ss'; % reformat to hours and minutes

%% print a message saying how long did the processing take
filename = 'time_stamps.txt';
message = sprintf('%s: %s %s %s',date,'Normalization (with brainshift correction) of fourteen patients took',string(sec),'(hh:mm:ss).');
% read any preceding timestamps if they exist
if isfile(filename)
    old_text = importdata(filename);
    new_text = [old_text; message];
else
    new_text = {message};
end
% write time info into a text file
writecell(new_text,filename);

%% set-up normalization options for Lead-DBS
% - Lead-DBS Job created on 04-Jan-2023 18:42:42 -
% --------------------------------------
function options = getoptslocal
    options.endtolerance = 10;
    options.sprungwert = 4;
    options.refinesteps = 0;
    options.tra_stdfactor = 0.9;
    options.cor_stdfactor = 1;
    options.earoot = '/Users/josefmana/Documents/leaddbs/';
    options.dicomimp.do = 0;
    options.dicomimp.method = 1;
    options.assignnii = 0;
    options.normalize.do = true;
    options.normalize.settings = [];
    options.normalize.method = 'ea_normalize_ants';
    options.normalize.methodn = 9;
    options.normalize.check = false;
    options.normalize.refine = 0;
    options.coregmr.check = 0;
    options.coregmr.method = 'SPM';
    options.coregmr.do = 0;
    options.overwriteapproved = 0;
    options.coregct.do = false;
    options.coregct.method = 'ea_coregctmri_ants';
    options.coregct.methodn = 7;
    options.verbose = 3;
    options.sides = [1 2];
    options.doreconstruction = false;
    options.maskwindow = 10;
    options.automask = 1;
    options.autoimprove = 0;
    options.axiscontrast = 8;
    options.zresolution = 10;
    options.atl.genpt = false;
    options.atl.normalize = 0;
    options.atl.can = true;
    options.atl.pt = 0;
    options.atl.ptnative = false;
    options.native = 0;
    options.d2.col_overlay = 1;
    options.d2.con_overlay = 1;
    options.d2.con_color = [1 1 1];
    options.d2.lab_overlay = 0;
    options.d2.bbsize = 50;
    options.d2.backdrop = 'MNI_ICBM_2009b_NLIN_ASYM T1';
    options.d2.fid_overlay = 1;
    options.d2.write = false;
    options.d2.atlasopacity = 0.15;
    options.d2.writeatlases = 1;
    options.manualheightcorrection = false;
    options.scrf.do = 1;
    options.scrf.mask = 2;
    options.d3.write = false;
    options.d3.prolong_electrode = 2;
    options.d3.verbose = 'on';
    options.d3.elrendering = 1;
    options.d3.exportBB = 0;
    options.d3.hlactivecontacts = 0;
    options.d3.showactivecontacts = 1;
    options.d3.showpassivecontacts = 1;
    options.d3.showisovolume = 0;
    options.d3.isovscloud = 0;
    options.d3.mirrorsides = 0;
    options.d3.autoserver = 0;
    options.d3.expdf = 0;
    options.d3.writeatlases = 1;
    options.numcontacts = 4;
    options.entrypoint = 'STN, GPi or ViM';
    options.entrypointn = 1;
    options.writeoutpm = 1;
    options.elmodeln = 1;
    options.elmodel = 'Medtronic 3389';
    options.atlasset = 'DISTAL Minimal (Ewert 2017)';
    options.atlassetn = 21;
    options.reconmethod = 'PaCER (Husch 2017)';
    options.expstatvat.do = 0;
    options.fiberthresh = 10;
    options.writeoutstats = 1;
    %%
    options.colormap = [0.2422 0.1504 0.6603
                        0.250390476190476 0.164995238095238 0.707614285714286
                        0.257771428571429 0.181780952380952 0.751138095238095
                        0.264728571428571 0.197757142857143 0.795214285714286
                        0.270647619047619 0.21467619047619 0.836371428571429
                        0.275114285714286 0.234238095238095 0.870985714285714
                        0.2783 0.255871428571429 0.899071428571429
                        0.280333333333333 0.278233333333333 0.9221
                        0.281338095238095 0.300595238095238 0.941376190476191
                        0.281014285714286 0.322757142857143 0.957885714285714
                        0.279466666666667 0.344671428571429 0.971676190476191
                        0.275971428571429 0.366680952380952 0.982904761904762
                        0.269914285714286 0.3892 0.9906
                        0.260242857142857 0.412328571428571 0.995157142857143
                        0.244033333333333 0.435833333333333 0.998833333333333
                        0.220642857142857 0.460257142857143 0.997285714285714
                        0.196333333333333 0.484719047619048 0.989152380952381
                        0.183404761904762 0.507371428571429 0.979795238095238
                        0.178642857142857 0.528857142857143 0.968157142857143
                        0.176438095238095 0.549904761904762 0.952019047619048
                        0.168742857142857 0.570261904761905 0.935871428571428
                        0.154 0.5902 0.9218
                        0.146028571428571 0.609119047619048 0.907857142857143
                        0.13802380952381 0.627628571428572 0.897290476190476
                        0.124814285714286 0.645928571428571 0.888342857142857
                        0.111252380952381 0.6635 0.876314285714286
                        0.0952095238095238 0.679828571428571 0.859780952380952
                        0.0688714285714285 0.694771428571429 0.839357142857143
                        0.0296666666666667 0.708166666666667 0.816333333333333
                        0.00357142857142857 0.720266666666667 0.7917
                        0.00665714285714287 0.731214285714286 0.766014285714286
                        0.0433285714285715 0.741095238095238 0.739409523809524
                        0.096395238095238 0.75 0.712038095238095
                        0.140771428571429 0.7584 0.684157142857143
                        0.1717 0.766961904761905 0.655442857142857
                        0.193766666666667 0.775766666666667 0.6251
                        0.216085714285714 0.7843 0.5923
                        0.246957142857143 0.791795238095238 0.556742857142857
                        0.290614285714286 0.797290476190476 0.518828571428572
                        0.340642857142857 0.8008 0.478857142857143
                        0.3909 0.802871428571428 0.435447619047619
                        0.445628571428572 0.802419047619048 0.390919047619048
                        0.5044 0.7993 0.348
                        0.561561904761905 0.794233333333333 0.304480952380953
                        0.617395238095238 0.787619047619048 0.261238095238095
                        0.671985714285714 0.779271428571429 0.2227
                        0.7242 0.769842857142857 0.191028571428571
                        0.773833333333333 0.759804761904762 0.164609523809524
                        0.820314285714286 0.749814285714286 0.153528571428571
                        0.863433333333333 0.7406 0.159633333333333
                        0.903542857142857 0.733028571428571 0.177414285714286
                        0.939257142857143 0.728785714285714 0.209957142857143
                        0.972757142857143 0.729771428571429 0.239442857142857
                        0.995647619047619 0.743371428571429 0.237147619047619
                        0.996985714285714 0.765857142857143 0.219942857142857
                        0.995204761904762 0.789252380952381 0.202761904761905
                        0.9892 0.813566666666667 0.188533333333333
                        0.978628571428571 0.838628571428572 0.176557142857143
                        0.967647619047619 0.8639 0.164290476190476
                        0.961009523809524 0.889019047619048 0.153676190476191
                        0.959671428571429 0.913457142857143 0.142257142857143
                        0.962795238095238 0.937338095238095 0.126509523809524
                        0.969114285714286 0.960628571428571 0.106361904761905
                        0.9769 0.9839 0.0805];
    %%
    options.dolc = 0;
    options.ecog.extractsurface.do = 0;
    options.ecog.extractsurface.method = 1;
    %%
    options.uipatdirs = {
                         '/Users/josefmana/Desktop/crusades/dbs_mriCONN_replicate/data/mri/IPN196'
                         '/Users/josefmana/Desktop/crusades/dbs_mriCONN_replicate/data/mri/IPN261'
                         '/Users/josefmana/Desktop/crusades/dbs_mriCONN_replicate/data/mri/IPN318'
                         '/Users/josefmana/Desktop/crusades/dbs_mriCONN_replicate/data/mri/IPN327'
                         '/Users/josefmana/Desktop/crusades/dbs_mriCONN_replicate/data/mri/IPN334'
                         '/Users/josefmana/Desktop/crusades/dbs_mriCONN_replicate/data/mri/IPN343'
                         '/Users/josefmana/Desktop/crusades/dbs_mriCONN_replicate/data/mri/IPN347'
                         '/Users/josefmana/Desktop/crusades/dbs_mriCONN_replicate/data/mri/IPN388'
                         '/Users/josefmana/Desktop/crusades/dbs_mriCONN_replicate/data/mri/IPN390'
                         '/Users/josefmana/Desktop/crusades/dbs_mriCONN_replicate/data/mri/IPN526'
                         '/Users/josefmana/Desktop/crusades/dbs_mriCONN_replicate/data/mri/IPN541'
                         '/Users/josefmana/Desktop/crusades/dbs_mriCONN_replicate/data/mri/IPN584'
                         '/Users/josefmana/Desktop/crusades/dbs_mriCONN_replicate/data/mri/IPN588'
                         '/Users/josefmana/Desktop/crusades/dbs_mriCONN_replicate/data/mri/IPN597'
                         }';
    %%
    options.leadprod = 'dbs';
    options.prefs.dev.profile = 'user';
    options.prefs.pp.do = 0;
    options.prefs.pp.csize = 4;
    options.prefs.pp.profile = 'local';
    options.prefs.prenii_searchstring = 'anat_*.nii';
    options.prefs.prenii_order = {
                                  't1'
                                  't2'
                                  'pd'
                                  }';
    options.prefs.prenii_unnormalized = 'anat_t2.nii';
    options.prefs.prenii_unnormalized_t1 = 'anat_t1.nii';
    options.prefs.prenii_unnormalized_pd = 'anat_pd.nii';
    options.prefs.tranii_unnormalized = 'postop_tra.nii';
    options.prefs.sagnii_unnormalized = 'postop_sag.nii';
    options.prefs.cornii_unnormalized = 'postop_cor.nii';
    options.prefs.rawctnii_unnormalized = 'postop_ct.nii';
    options.prefs.ctnii_coregistered = 'rpostop_ct.nii';
    options.prefs.tp_ctnii_coregistered = 'tp_rpostop_ct.nii';
    options.prefs.preferMRCT = 1;
    options.prefs.patientdir = '';
    options.prefs.gprenii = 'glanat.nii';
    options.prefs.gtranii = 'glpostop_tra.nii';
    options.prefs.gcornii = 'glpostop_cor.nii';
    options.prefs.gsagnii = 'glpostop_sag.nii';
    options.prefs.gctnii = 'glpostop_ct.nii';
    options.prefs.tp_gctnii = 'tp_glpostop_ct.nii';
    options.prefs.bids_session_postop = 'ses-postDBS';
    options.prefs.bids_session_preop = 'ses-preDBS';
    options.prefs.tonemap = 'heuristic';
    options.prefs.rest_searchstring = 'rest*.nii';
    options.prefs.rest = 'rest.nii';
    options.prefs.lc.struc.maxdist = 2;
    options.prefs.lc.struc.minlen = 3;
    options.prefs.lc.graphsurfc = [0.2081 0.1663 0.5292];
    options.prefs.lc.matsurfc = [0.8 0.7 0.4];
    options.prefs.lc.seedsurfc = [0.8 0.1 0.1];
    options.prefs.lc.func.regress_global = 1;
    options.prefs.lc.func.regress_wmcsf = 1;
    options.prefs.lc.func.bphighcutoff = 0.08;
    options.prefs.lc.func.bplowcutoff = 0.009;
    options.prefs.lc.datadir = '/Users/josefmana/Documents/leaddbs/connectomes/';
    options.prefs.lc.defaultParcellation = 'Automated Anatomical Labeling 3 (Rolls 2020)';
    options.prefs.lcm.vatseed = 'binary';
    options.prefs.lcm.vat2fmrimethod = 'fsl';
    options.prefs.lcm.chunk = 10;
    options.prefs.lcm.includesurf = 1;
    options.prefs.lcm.struc.patienttracts.nativeseed = 0;
    options.prefs.b0 = 'b0.nii';
    options.prefs.fa = 'fa.nii';
    options.prefs.fa2anat = 'fa2anat.nii';
    options.prefs.FTR_unnormalized = 'FTR.mat';
    options.prefs.FTR_normalized = 'wFTR.mat';
    options.prefs.DTD = 'DTD.mat';
    options.prefs.HARDI = 'HARDI.mat';
    options.prefs.dti = 'dti.nii';
    options.prefs.bval = 'dti.bval';
    options.prefs.bvec = 'dti.bvec';
    options.prefs.sampledtidicom = 'sample_dti_dicom.dcm';
    options.prefs.normmatrix = 'lmat.txt';
    options.prefs.normalize.default = 'ea_normalize_ants';
    options.prefs.normalize.inverse.warp = 'inverse';
    options.prefs.normalize.inverse.customtpm = 0;
    options.prefs.normalize.createwarpgrids = 0;
    options.prefs.normalize.fsl.warpres = 8;
    options.prefs.normalize.spm.resolution = 1;
    options.prefs.reco.mancoruse = 'postop';
    options.prefs.reco.saveACPC = 0;
    options.prefs.reco.saveimg = 0;
    options.prefs.reco.exportfiducials = 0;
    options.prefs.ctcoreg.default = 'ea_coregctmri_ants';
    options.prefs.mrcoreg.default = 'spm';
    options.prefs.mrcoreg.writeoutcoreg = 0;
    options.prefs.scrf.tonemap = 'tp_';
    options.prefs.lg.defaultParcellation = 'Automated Anatomical Labeling 3 (Rolls 2020)';
    options.prefs.hullmethod = 2;
    options.prefs.hullsmooth = 5;
    options.prefs.hullsimplify = 0.3;
    options.prefs.lhullmethod = 2;
    options.prefs.lhullsmooth = 3;
    options.prefs.lhullsimplify = 'auto';
    options.prefs.d2.useprepost = 'pre';
    options.prefs.d2.groupcolors = 'lead';
    options.prefs.d2.isovolsmoothed = 's';
    options.prefs.d2.isovolcolormap = 'jet';
    options.prefs.d2.isovolsepcomb = 'combined';
    options.prefs.d3.fiberstyle = 'tube';
    options.prefs.d3.fiberdiameter = 0.1;
    options.prefs.d3.maxfibers = 200;
    options.prefs.d3.colorjitter = 0;
    options.prefs.d3.showdirarrows = 0;
    options.prefs.d3.fiber_activated_color = [1 0 0];
    options.prefs.d3.fiber_nonactivated_color = [1 1 1];
    options.prefs.d3.fiber_damaged_color = [0.5 0 0.5];
    options.prefs.d3.fiber_csf_color = [0 0 1];
    options.prefs.d3.fiber_outside_color = [0 1 0];
    options.prefs.d3.pointcloudstyle = 'plain';
    options.prefs.d3.cortexcolor = [0.65 0.65 0.65];
    options.prefs.d3.cortexalpha = 0.5;
    options.prefs.d3.cortex_defaultatlas = 'DKT';
    options.prefs.d3.fs.dev = 0;
    options.prefs.video.path = [-90 10
                                -110 10
                                -180 80
                                -250 10
                                -360 10
                                -450 10];
    options.prefs.video.opts.FrameRate = 24;
    options.prefs.video.opts.Duration = 30;
    options.prefs.video.opts.Periodic = true;
    options.prefs.vat.gm = 'mask';
    options.prefs.vat.efieldmax = 10000;
    options.prefs.mer.rejwin = [1 60];
    options.prefs.mer.offset = 2;
    options.prefs.mer.length = 24;
    options.prefs.mer.markersize = 0.5;
    options.prefs.mer.defaulttract = 1;
    options.prefs.mer.n_pnts = 50;
    options.prefs.mer.tag.visible = 'off';
    options.prefs.mer.step_size = [0.25 0.75 0.05];
    options.prefs.mer.tract_info(1).label = 'central';
    options.prefs.mer.tract_info(1).color = [0.5 0 0];
    options.prefs.mer.tract_info(1).position = [0 0 0];
    options.prefs.mer.tract_info(2).label = 'anterior';
    options.prefs.mer.tract_info(2).color = [0.5 0.5 0];
    options.prefs.mer.tract_info(2).position = [0 1 0];
    options.prefs.mer.tract_info(3).label = 'posterior';
    options.prefs.mer.tract_info(3).color = [0 0.5 0];
    options.prefs.mer.tract_info(3).position = [0 -1 0];
    options.prefs.mer.tract_info(4).label = 'lateral';
    options.prefs.mer.tract_info(4).color = [0.5 0 0.5];
    options.prefs.mer.tract_info(4).position = [1 0 0];
    options.prefs.mer.tract_info(5).label = 'medial';
    options.prefs.mer.tract_info(5).color = [0 0.5 0.5];
    options.prefs.mer.tract_info(5).position = [-1 0 0];
    options.prefs.dicom.dicomfiles = 0;
    options.prefs.dicom.tool = 'dcm2niix';
    options.prefs.addfibers = {};
    options.prefs.native.warp = 'inverse';
    options.prefs.ls.autosave = 0;
    options.prefs.ls.dir = '';
    options.prefs.env.dev = 0;
    options.prefs.env.logtime = 0;
    options.prefs.env.campus = 'generic';
    options.prefs.ixi.meanage = 60;
    options.prefs.ixi.dir = '';
    options.prefs.ltx.pdfconverter = '';
    options.prefs.genetics.dbdir = '/Users/josefmana/Documents/leaddbs/templates/space/MNI_ICBM_2009b_NLIN_ASYM/genetics/';
    options.prefs.platform.glnxa64.load_shipped_runtime = false;
    options.prefs.platform.maci64.load_shipped_runtime = false;
    options.prefs.platform.win64.load_shipped_runtime = false;
    options.prefs.firstrun = 'off';
    options.prefs.slicer.dir = '/Applications/Slicer.app';
    options.prefs.machine.atlaspresets = struct([]);
    options.prefs.machine.checkreg.default = 'DISTAL Minimal (Ewert 2017)@STN';
    options.prefs.machine.chirp = 0;
    options.prefs.machine.d2.col_overlay = 1;
    options.prefs.machine.d2.con_overlay = 1;
    options.prefs.machine.d2.con_color = [1 1 1];
    options.prefs.machine.d2.lab_overlay = 0;
    options.prefs.machine.d2.bbsize = 50;
    options.prefs.machine.d2.backdrop = 'MNI_ICBM_2009b_NLIN_ASYM T1';
    options.prefs.machine.d2.fid_overlay = 1;
    options.prefs.machine.lc.general.parcellation = 'Automated Anatomical Labeling 3 (Rolls 2020)';
    options.prefs.machine.lc.graph.struc_func_sim = 0;
    options.prefs.machine.lc.graph.nodal_efficiency = 0;
    options.prefs.machine.lc.graph.eigenvector_centrality = 0;
    options.prefs.machine.lc.graph.degree_centrality = 0;
    options.prefs.machine.lc.graph.fthresh = NaN;
    options.prefs.machine.lc.graph.sthresh = NaN;
    options.prefs.machine.lc.func.compute_CM = 0;
    options.prefs.machine.lc.func.compute_GM = 0;
    options.prefs.machine.lc.func.prefs.TR = 2.69;
    options.prefs.machine.lc.struc.compute_CM = 0;
    options.prefs.machine.lc.struc.compute_GM = 0;
    options.prefs.machine.lc.struc.ft.method = 'ea_ft_gqi_yeh';
    options.prefs.machine.lc.struc.ft.do = 0;
    options.prefs.machine.lc.struc.ft.normalize = 0;
    options.prefs.machine.lc.struc.ft.dsistudio.fiber_count = 200000;
    options.prefs.machine.lg = struct([]);
    options.prefs.machine.methods_show = 0;
    options.prefs.machine.normsettings.maget_peerset = 'IXI-Dataset';
    options.prefs.machine.normsettings.maget_peersetcell = [];
    options.prefs.machine.normsettings.maget_atlasset = 'DISTAL (Ewert 2016)';
    options.prefs.machine.normsettings.schoenecker_movim = 1;
    options.prefs.machine.normsettings.ants_preset = 'ea_antspreset_effective_lowvar_default';
    options.prefs.machine.normsettings.ants_scrf = 1;
    options.prefs.machine.normsettings.ants_strategy = 'SyN';
    options.prefs.machine.normsettings.ants_metric = 'Mutual Information';
    options.prefs.machine.normsettings.ants_numcores = '2';
    options.prefs.machine.normsettings.ants_stagesep = 0;
    options.prefs.machine.normsettings.fsl_skullstrip = 0;
    options.prefs.machine.normsettings.ants_usefa = 0;
    options.prefs.machine.normsettings.ants_skullstripped = 0;
    options.prefs.machine.normsettings.spmnewseg_scalereg = 1;
    options.prefs.machine.normsettings.ants_reinforcetargets = 0;
    options.prefs.machine.normsettings.ants_usepreexisting = 1;
    options.prefs.machine.space = 'MNI_ICBM_2009b_NLIN_ASYM';
    options.prefs.machine.togglestates.cutview = '3d';
    options.prefs.machine.togglestates.refreshcuts = 0;
    options.prefs.machine.togglestates.refreshview = 0;
    options.prefs.machine.togglestates.xyzmm = [-30 -30 -30];
    options.prefs.machine.togglestates.xyztoggles = [0 1 1];
    options.prefs.machine.togglestates.xyztransparencies = [100 100 100];
    options.prefs.machine.togglestates.template = 'MNI_ICBM_2009b_NLIN_ASYM T1 (Fonov 2011)';
    options.prefs.machine.togglestates.tinvert = 0;
    options.prefs.machine.togglestates.customfile = [];
    options.prefs.machine.vatsettings.estimateInTemplate = 1;
    options.prefs.machine.vatsettings.oss_dbs.installed = 0;
    options.prefs.machine.vatsettings.butenko_ethresh = 0.2;
    options.prefs.machine.vatsettings.dembek_pw = 60;
    options.prefs.machine.vatsettings.dembek_ethresh = 0.2;
    options.prefs.machine.vatsettings.dembek_ethreshpw = 60;
    options.prefs.machine.vatsettings.fastfield_cb = 0.1;
    options.prefs.machine.vatsettings.fastfield_ethresh = 0.2;
    options.prefs.machine.vatsettings.horn_cwm = 0.14;
    options.prefs.machine.vatsettings.horn_useatlas = 1;
    options.prefs.machine.vatsettings.horn_atlasset = 'DISTAL Minimal (Ewert 2017)';
    options.prefs.machine.vatsettings.horn_cgm = 0.33;
    options.prefs.machine.vatsettings.horn_ethresh = 0.2;
    options.prefs.machine.vatsettings.horn_removeElectrode = 1;
    options.prefs.machine.vatsettings.butenko_fiberDiameter = 5.7;
    options.prefs.machine.vatsettings.butenko_calcAxonActivation = 0;
    options.prefs.machine.vatsettings.butenko_connectome = 'HCP_MGH_32fold_groupconnectome (Horn 2017)';
    options.prefs.machine.vatsettings.butenko_axonLength = 10;
    options.prefs.machine.vatsettings.butenko_interactive = 0;
    options.prefs.machine.vatsettings.butenko_useTensorData = 1;
    options.prefs.machine.vatsettings.butenko_tensorFileName = 'IITmean_tensor.nii.gz';
    options.prefs.machine.view.az = 180;
    options.prefs.machine.view.el = 21;
    options.prefs.machine.view.camva = 9.25958075788405;
    options.prefs.machine.view.camup = [0 0 1];
    options.prefs.machine.view.camproj = 'orthographic';
    options.prefs.machine.view.camtarget = [-3.15548027321493 -25.2737400702356 -3.64952824374033];
    options.prefs.machine.view.campos = [1.66783869745351 432.619292151714 170.751216175643];
    options.prefs.machine.defaultatlas = 'DISTAL Minimal (Ewert 2017)';
    options.prefs.prenii = 'glanat.nii';
    options.prefs.tranii = 'glpostop_tra.nii';
    options.prefs.cornii = 'glpostop_cor.nii';
    options.prefs.sagnii = 'glpostop_sag.nii';
    options.prefs.ctnii = 'glpostop_ct.nii';
    options.lc.general.parcellation = 'Automated Anatomical Labeling 3 (Rolls 2020)';
    options.lc.graph.struc_func_sim = 0;
    options.lc.graph.nodal_efficiency = 0;
    options.lc.graph.eigenvector_centrality = 0;
    options.lc.graph.degree_centrality = 0;
    options.lc.graph.fthresh = NaN;
    options.lc.graph.sthresh = NaN;
    options.lc.func.compute_CM = 0;
    options.lc.func.compute_GM = 0;
    options.lc.func.prefs.TR = 2.69;
    options.lc.struc.compute_CM = 0;
    options.lc.struc.compute_GM = 0;
    options.lc.struc.ft.method = 'ea_ft_gqi_yeh';
    options.lc.struc.ft.do = 0;
    options.lc.struc.ft.normalize = 0;
    options.lc.struc.ft.dsistudio.fiber_count = 200000;
    options.exportedJob = 1;
end
