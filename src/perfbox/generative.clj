(ns perfbox.generative
  (:require
   [zen.core :as zen]
   [org.httpkit.client :as http]
   [cheshire.core]
   [clojure.string :as str]
   [clojure.walk]
   [prometheus.core])
  (:import [java.util UUID]))



(def requests
  "GET	/AllergyIntolerance?category=food&criticality=low&date=1971-05-12
GET	/AllergyIntolerance?clinical-status=active&type=allergy
GET	/AllergyIntolerance?clinical-status=http://terminology.hl7.org/CodeSystem/allergyintolerance-clinical|active
GET	/AllergyIntolerance/
GET	/AllergyIntolerance/81be86e9-6155-163d-d6ae-9c03ae1e0765
GET	/CarePlan?category=assess-plan&category=http://hl7.org/fhir/us/core/CodeSystem/careplan-category|assess-plan
GET	/CarePlan?intent=order&category=http://hl7.org/fhir/us/core/CodeSystem/carelan-category|assess-plan&category=assess-plan
GET	/CarePlan?subject=Patient/24dbfed4-3099-0a1d-3fe6-68c3f953ccb0
GET	/CarePlan/
GET	/CarePlan/ea83893a-4829-6a43-cc29-a0c397409dfa
GET	/CareTeam?subject=Patient/24dbfed4-3099-0a1d-3fe6-68c3f953ccb0
GET	/CareTeam?subject=Patient/24dbfed4-3099-0a1d-3fe6-68c3f953ccb0&status=active
GET	/CareTeam/
GET	/CareTeam/5abae625-094b-4d84-0f32-6a23ab7acfdd
GET	/Condition/
GET	/Condition?code=160903007
GET	/Condition?recorded-date=2010-04-11&category=http://terminology.hl7.org/CodeSystem/condition-category|encounter-diagnosis&encounter=Encounter/6020c269-9298-2d5f-31b5-5b8a1c0dc4a2
GET	/Condition?verification-status=confirmed&code=160903007
GET	/Condition/cf795c86-b5e2-2421-08ef-a84ef1806115
GET	/Device?status=active
GET	/Device?status=active&patient=Patient/25390467-fbb6-73c1-590b-990079dd1f17
GET	/Device?type=170615005&patient=Patient/25390467-fbb6-73c1-590b-990079dd1f17&status=active
GET	/Device/
GET	/Device/afa9de61-bdce-2286-0bd8-1e55e1562dc4
GET	/DiagnosticReport?code=34117-2
GET	/DiagnosticReport?encounter=Encounter/a4628616-a70d-e7a4-b6f5-96252a8c74a8&code=34117-2
GET	/DiagnosticReport?encounter=Encounter/a4628616-a70d-e7a4-b6f5-96252a8c74a8&subject=Patient/1aceb71a-79af-5ff6-04aa-b8824b986256&code=http://loinc.org|34117-2
GET	/DiagnosticReport/
GET	/DiagnosticReport/91ecea5c-4c5d-9c4b-3679-f5a146ad84ff
GET	/DocumentReference?subject=Patient/d6480193-5b2a-038a-06a8-9a7b1697f63d&type=34117-2
GET	/DocumentReference?type=34117-2
GET	/DocumentReference?type=34117-2&category=clinical-note&category=http://hl7.org/fhir/us/core/CodeSystem/us-core-documentreference-category|clinical-note
GET	/DocumentReference/
GET	/DocumentReference/bde025cb-b795-2dfa-eb08-d67ae22f011f
GET	/ExplanationOfBenefit/
GET	/ExplanationOfBenefit?status=active&type=institutional
GET	/ExplanationOfBenefit?type=institutional
GET	/ExplanationOfBenefit?type=institutional&patient=Patient/d39838e4-c514-265d-126b-0188bd903ffd&created=2004-11-13
GET	/ExplanationOfBenefit/438ec6c8-8893-5f61-a6bf-aea622cb2799
GET	/ImagingStudy/
GET	/ImagingStudy/8b25b208-da9c-b581-6105-cb3393c6e97d
GET	/Immunization?vaccine-code=140
GET	/Immunization?vaccine-code=140&status=completed&vaccine-code=http://hl7.org/fhir/sid/cvx|140
GET	/Immunization?vaccine-code=http://hl7.org/fhir/sid/cvx|140&status=completed
GET	/Immunization/
GET	/Immunization/fcf794c3-39c7-a146-db8d-b61c976653af
GET	/Medication?code=http://www.nlm.nih.gov/research/umls/rxnorm|1659131
GET	/Medication?status=active&code=1659131
GET	/Medication/
GET	/Medication/d11b10f7-6b40-9e15-defd-1537c08d5d49
GET	/MedicationAdministration?subject=Patient/24dbfed4-3099-0a1d-3fe6-68c3f953ccb0
GET	/MedicationAdministration?subject=Patient/24dbfed4-3099-0a1d-3fe6-68c3f953ccb0&status=completed
GET	/MedicationAdministration/
GET	/MedicationAdministration/a1c5c3d3-a0e7-45aa-c150-72f5200d5287
GET	/MedicationRequest?authoredon=1999-04-29&subject=Patient/012609c5-0dcf-f058-f8cf-9190ea4dff9d&encounter=Encounter/fef41b3f-9266-2f67-8aa0-fc63813a13b9
GET	/MedicationRequest?encounter=Encounter/fef41b3f-9266-2f67-8aa0-fc63813a13b9&status=stopped
GET	/MedicationRequest?intent=order
GET	/MedicationRequest/
GET	/MedicationRequest/b420744d-72f4-c4e6-e28f-6bee45892685
GET	/Observation?category=survey
GET	/Observation?category=survey&combo-code=http://loinc.org|55758-7&subject=Patient/3810294d-f1ec-ed71-4173-e21f66123c47
GET	/Observation/
GET	/Observation/f1b2418c-403b-aafe-fd03-71189216e7b7
GET	/Provenance?target=Patient/d1a48bab-db9d-8b45-2d1a-a74b252c3cb9
GET	/Provenance/
GET	/Provenance/b73dd8ef-a941-7229-a19c-66c91be97409
GET	/SupplyDelivery/
GET	/SupplyDelivery/ac120c69-df29-4fa5-fcae-bbb36ee4ef5d")

(defn parse-params [x]
  (when x
    (->> (str/split x #"&")
         (reduce (fn [acc x]
                   (let [[k v] (str/split x #"=")]
                     (assoc acc (keyword k) v)))
                 {}))))

(defn request [ctx req]
  (let [start (System/currentTimeMillis)
        resp @(http/request
          {:method :get
           :url (str (:host ctx) (:url req))
           :basic-auth (:basic-auth ctx)
           :query-params (merge {} (:query-params req))})]
    (-> resp
        (update :body (fn [x]
                        (when x
                          (cheshire.core/parse-string x keyword))))
        (assoc :d (- (System/currentTimeMillis) start)))))

(comment
  
  



  (def ctx {:host  "http://aidbox.innovacer.aidbox.io"
            :basic-auth ["root" "secret"]})

  (def resp 
    (request ctx
      {:method :get
       :url "/ExplanationOfBenefit?type=institutional"
       :query-params {:_count 1}}))

  (:status resp)
  (:body resp)
  (:d resp)
  

  (def reqs 
    (->> (str/split requests #"\n")
         (mapv (fn [x]
                 (let [[m url] (str/split x #"\s+" 2)
                       [path params] (str/split url #"\?")]
                   {:method (keyword (str/lower-case m))
                    :url path
                    :query-params (parse-params params)})))))
  reqs

  (def resps 
    (->> reqs
         (reduce (fn [acc req]
                   (let [params (merge (:query-params req) {:_timeout 2})
                         resp (request ctx (-> req (assoc :query-params params)))]
                     (println (:status resp) (:d resp) (:url req) params (:error resp))
                     (conj acc (assoc resp :request req))))
                 [])))

  (->> resps
       (filter (fn [x] (= 500 (:status x))))
       (mapv
        (fn [x]
          (assoc (:request x) :status (:status x)))))

  
  




  )
