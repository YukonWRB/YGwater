CREATE OR REPLACE FUNCTION discrete.guideline_pb(x_id integer)
    RETURNS float8
    LANGUAGE plpgsql
AS $function$
DECLARE
    Hard_T NUMERIC;
    Hard_D NUMERIC;
    Ca_T NUMERIC;
    Ca_D NUMERIC;
    Mg_T NUMERIC;
    Mg_D NUMERIC;
    H NUMERIC; -- hardness calculated w.r.t. specific guidleine

BEGIN
    SELECT results.result INTO Hard_T FROM results WHERE results.sample_id = x_id AND results.parameter_id = 100 AND results.sample_fraction = 19;
    SELECT results.result INTO Hard_D FROM results WHERE results.sample_id = x_id AND results.parameter_id = 100 AND results.sample_fraction = 5;
    SELECT results.result INTO Ca_T FROM results WHERE results.sample_id = x_id AND results.parameter_id = 1061 AND results.sample_fraction = 10;
    SELECT results.result INTO Ca_D FROM results WHERE results.sample_id = x_id AND results.parameter_id = 1061 AND results.sample_fraction = 5;
    SELECT results.result INTO Mg_T FROM results WHERE results.sample_id = x_id AND results.parameter_id = 1103 AND results.sample_fraction = 19;
    SELECT results.result INTO Mg_D FROM results WHERE results.sample_id = x_id AND results.parameter_id = 1103 AND results.sample_fraction = 5;

    IF (Ca_D IS NOT NULL AND Mg_D IS NOT NULL) AND (Ca_D * Mg_D > 0) THEN
        H := (2.497*Ca_D+4.118 *Mg_D);
    ELSIF (Hard_D IS NOT NULL) AND (Hard_D > 0) THEN
        H := Hard_D;
    ELSEIF (Ca_T IS NOT NULL AND Mg_T IS NOT NULL) AND (Ca_T * Mg_T > 0) THEN
        H := (2.497*Ca_T+4.118 *Mg_T);
    ELSEIF (Hard_T IS NOT NULL) AND (Hard_T > 0) THEN
        H := Hard_T;
    ELSE
        H := 0.001; -- default value (mg/L)
    END IF;
    RETURN H;
END;
$function$;




* Contaminated Sites, Schedule 3 (water) Aquatic Life freshwater for Cadmium* converts from Âµg/L to mg/L* Updated 2022/11/02 by Marie Ducharme#Z=1#S=3H = AH = [XHard] > 0 ? [XHard]H = [Hard-T] > 0 ? [Hard-T]H = [Ca-T] * [Mg-T] > 0 ? (2.497*[Ca-T])+(4.118 * [Mg-T]) H = [Hard-D] > 0 ? [Hard-D]H = [Ca-D] * [Mg-D] > 0 ? (2.497*[Ca-D])+(4.118 *[Mg-D]) X = H = A ? "Hard missing"X = H < 30 ? 0.0001X = H < 90 ? 0.0003X = H <150 ? 0.0005X = H <=210 ? 0.0006


CREATE OR REPLACE FUNCTION dev.aquacache_wqgl/guideline_pb(x_id integer)
    RETURNS float8
    LANGUAGE plpgsql
AS $function$
DECLARE
    Hard_T NUMERIC;
    Hard_D NUMERIC;
    Ca_T NUMERIC;
    Ca_D NUMERIC;
    Mg_T NUMERIC;
    Mg_D NUMERIC;
    H NUMERIC; -- hardness calculated w.r.t. specific guidleine

BEGIN

    SELECT results.result INTO Hard_T FROM results WHERE results.sample_id = x_id AND results.parameter_id = 100 AND results.sample_fraction = 19;
    SELECT results.result INTO Hard_D FROM results WHERE results.sample_id = x_id AND results.parameter_id = 100 AND results.sample_fraction = 5;
    SELECT results.result INTO Ca_T FROM results WHERE results.sample_id = x_id AND results.parameter_id = 1061 AND results.sample_fraction = 10;
    SELECT results.result INTO Ca_D FROM results WHERE results.sample_id = x_id AND results.parameter_id = 1061 AND results.sample_fraction = 5;
    SELECT results.result INTO Mg_T FROM results WHERE results.sample_id = x_id AND results.parameter_id = 1103 AND results.sample_fraction = 19;
    SELECT results.result INTO Mg_D FROM results WHERE results.sample_id = x_id AND results.parameter_id = 1103 AND results.sample_fraction = 5;

    IF (Ca_D IS NOT NULL AND Mg_D IS NOT NULL) AND (Ca_D * Mg_D > 0) THEN
        H := (2.497*Ca_D+4.118 *Mg_D);
    ELSIF (Hard_D IS NOT NULL) AND (Hard_D > 0) THEN
        H := Hard_D;
    ELSEIF (Ca_T IS NOT NULL AND Mg_T IS NOT NULL) AND (Ca_T * Mg_T > 0) THEN
        H := (2.497*Ca_T+4.118 *Mg_T);
    ELSEIF (Hard_T IS NOT NULL) AND (Hard_T > 0) THEN
        H := Hard_T;
    ELSE
        H := 0.001; -- default value (mg/L)
    END IF;

    IF H = 0 THEN
        RETURN 0.0001;
    ELSIF H < 30 THEN
        RETURN 0.0001;
    ELSIF H < 90 THEN
        RETURN 0.0003;
    ELSIF H < 150 THEN
        RETURN 0.0005;
    ELSIF H <= 210 THEN
        RETURN 0.0006;
    END IF;
END;