// Docs on event and context https://www.netlify.com/docs/functions/#the-handler-method
const axios = require("axios");
const { DB_API_KEY } = process.env;
// const Airtable = require("airtable");
// const base = new Airtable({
//   apiKey: DB_API_KEY,
// }).base("appUOSxxv2dDfXs4t");

exports.handler = async (event) => {
  console.log("change just for show");
  
  const url = "https://api.airtable.com/v0/appUOSxxv2dDfXs4t/9?";
  const config = { headers: { Authorization: `Bearer ${DB_API_KEY}` } };
  return axios
    .get(url, config)
    .then((resp) => {
      let data = resp.data.records;
      console.log(data);
      return {
        statusCode: 200,
        body: JSON.stringify({ data }),
      };
    })
    .catch((err) => {
      // console.log(err);
      return {
        statusCode: 500,
        body: JSON.stringify({ err }),
      };
    });

  // return base("9")
  //   .select({
  //     pageSize: 100,
  //     maxRecords: 1,
  //     view: "Grid view",
  //   })
  //   .firstPage((err, records) => {
  //     if (err) {
  //       return {
  //         statusCode: 500,
  //         body: JSON.stringify({ err }),
  //       };
  //     }

  //     // console.log(records);

  //     records.forEach((rec) => console.log(record.fetch("Name")));

  //     // records.forEach((rec) => {
  //     //   console.log(`Rec: ${record.get("Name")}`);
  //     // });

  //     return {
  //       statusCode: 200,
  //       body: JSON.stringify({ records }),
  //     };
  //   });
  // .eachPage(
  //   function page(records, fetchNextPage) {
  //     // This function (`page`) will get called for each page of records.

  //     // console.log("records");
  //     console.log(records);

  //     // records.forEach(function (record) {
  //     //   console.log("Retrieved", record.get("Name"));
  //     // });
  //     return {
  //       statusCode: 200,
  //       body: JSON.stringify({ message: `get-stats json`, records }),
  //       // // more keys you can return:
  //       // headers: { "headerName": "headerValue", ... },
  //       // isBase64Encoded: true,
  //     };

  //     // To fetch the next page of records, call `fetchNextPage`.
  //     // If there are more records, `page` will get called again.
  //     // If there are no more records, `done` will get called.
  //     // fetchNextPage();
  //   },
  //   function done(err) {
  //     if (err) {
  //       console.error(err);
  //       return;
  //     }

  //     console.log("done fn");
  //     return {
  //       statusCode: 200,
  //       body: JSON.stringify(records),
  //     };
  //   }
  // );
};
